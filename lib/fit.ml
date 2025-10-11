(* Christian Lindig <lindig@gmail.com>
 *)

open Angstrom

let defer finally = Fun.protect ~finally
let arch32 = match Sys.word_size with 32 -> true | _ -> false

(** redefine & as bitwise operation, && is still logical and *)
let ( & ) = Int.logand

let ( >> ) = Int.shift_right
let fail fmt = Printf.ksprintf fail fmt
let failwith fmt = Printf.ksprintf failwith fmt
let sprintf = Printf.sprintf

type arch = BE  (** Big Endian *) | LE  (** Little Endian *)

module Dict = Map.Make (Int)

module Type = struct
  (** A FIT file holds records of values. Each value has a type and a record is
      defined by the type of each value. This structure is defined in the FIT
      file itself. This module captures this type structure. *)

  (** An integer can be signed or unsigned *)
  type sign = Signed | Unsigned

  (** An invalid integer value is either denoted as zero (ZZ) or FF, depending
      on its type *)
  type invalid = ZZ | FF

  (** base types - a value has one of these types *)
  type base =
    | Enum
    | Bytes
    | String
    | Int of sign * int * invalid
    | Float of int  (** either 32 or 64 bits *)

  type field = {
      slot : int  (** position within record - defines purpose *)
    ; size : int  (** in bytes *)
    ; ty : base  (** representation *)
  }
  (** A record is composed of fields. A field has a size and type. The slot is a
      number that defines its purpose, like heart rate, and this meaning is
      assigned in the protocol and not in the binary stream itself *)

  type record = {
      msg : int
    ; arch : arch
    ; fields : field list
    ; dev_fields : int  (** total size in bytes of dev fields *)
  }
  (** A record is comprised of fields; the overall purpose of the record is
      captured by the msg number; the binary format of data in the fields
      respects the arch architecture. [dev_fields] are additional fields which
      we don't decode but just skip over *)

  let sum = List.fold_left ( + ) 0
  let size { size; _ } = size
  let total fs = fs |> List.map size |> sum

  let json { msg; arch; fields; dev_fields } =
    let t = function
      | Enum -> `String "enum"
      | Bytes -> `String "bytes"
      | String -> `String "string"
      | Int (Signed, bits, _) -> `String (sprintf "int%d" bits)
      | Int (Unsigned, bits, _) -> `String (sprintf "uint%d" bits)
      | Float bits -> `String (sprintf "float%d" bits)
    in
    let f { slot; size; ty } =
      `Assoc [ ("slot", `Int slot); ("size", `Int size); ("type", t ty) ]
    in
    `Assoc
      [
        ("msg", `Int msg)
      ; ("arch", `String (match arch with LE -> "LE" | BE -> "BE"))
      ; ("fields", `List (List.map f fields))
      ; ("dev_fields", `Int dev_fields)
      ; ("size", `Int (total fields + dev_fields))
      ]

  (** parse a [field] definition from a FIT file *)
  let field =
    let* slot = any_uint8 in
    let* size = any_uint8 in
    let* ty' = any_uint8 in
    let ty =
      match ty' & 0b1111 with
      | 0 -> Enum
      | 1 -> Int (Signed, 8, FF)
      | 2 -> Int (Unsigned, 8, FF)
      | 3 -> Int (Signed, 16, FF)
      | 4 -> Int (Unsigned, 16, FF)
      | 5 -> Int (Signed, 32, FF)
      | 6 -> Int (Unsigned, 32, FF)
      | 7 -> String
      | 8 -> Float 32
      | 9 -> Float 64
      | 10 -> Int (Unsigned, 8, ZZ)
      | 11 -> Int (Unsigned, 16, ZZ)
      | 12 -> Int (Unsigned, 32, ZZ)
      | 13 -> Bytes
      | _ -> failwith "unknown field base type (%s)" __LOC__
    in
    return { slot; size; ty }

  (** parse a [record] definition. We know ahead of time if the record defintion
      may contain development field definitions, which we then have to read as
      well *)
  let record ~dev =
    let* arch =
      int8 0 *> any_int8 >>= function
      | 0 -> return LE
      | 1 -> return BE
      | n -> fail "expected 0 or 1 in byte for endianness, found %d" n
    in
    let* msg = match arch with LE -> LE.any_uint16 | BE -> BE.any_uint16 in
    let* n = any_int8 in
    let* fields = count n field in
    let dev_fields =
      if dev then
        let* n = any_int8 in
        let* dev_fields = count n field in
        return dev_fields
      else return []
    in
    let* dev_fields = dev_fields in
    return { msg; arch; fields; dev_fields = total dev_fields }
end

type header = { protocol : int; profile : int; length : int }

(** A [value] represents a datum read from a FIT file. *)
type value =
  | Enum of int
  | String of string
  | Int of int
  | Int32 of Int32.t
  | Int64 of Int64.t
  | Float of float
  | Unknown

let to_string value =
  match value with
  | Enum d -> sprintf "enum(%d)" d
  | String s -> sprintf "string(%s)" s
  | Int i -> sprintf "int(%d)" i
  | Int32 i -> sprintf "int(%ld)" i
  | Int64 i -> sprintf "int(%Ld)" i
  | Float f -> sprintf "float(%f)" f
  | Unknown -> "unknown"

type record = { msg : int; fields : (int * value) list }
(** A [record] is a record of values read from a FIT file. Each value is in a
    slot, which is reported as [int] value. Slots are not consecutive and are
    part of the FIT protocol. *)

type t = { header : header; records : record list }
(** [t] represents the contents of a FIT file *)

(** [base] reads the next value of type [ty] from the stream and returns it as a
    [value]. The type [ty] is known because the caller knows the from the record
    definition the types of all value. Furthermore, the current endianness
    [arch] is known as well *)
let base arch ty =
  let ff = -1 in
  let ff' = Int32.minus_one in
  let float = function
    | x when Float.is_nan x -> return Unknown
    | x when x = Float.infinity -> return Unknown
    | x when x = Float.neg_infinity -> return Unknown
    | x -> return (Float x)
  in
  let int ukn x = return (if x = ukn then Unknown else Int x) in
  let uint8 unk x =
    let x = x land 0xff in
    return (if x = unk then Unknown else Int x)
  in
  let uint16 unk x =
    let x = x land 0xffff in
    return (if x = unk then Unknown else Int x)
  in
  let int32 ukn x =
    match arch32 with
    | _ when x = ukn -> return Unknown
    | true -> return (Int32 x)
    | false -> return (Int (Int32.to_int x))
  in
  let uint32 unk x =
    match arch32 with
    | _ when x = unk -> return Unknown
    | true -> return (Int64 Int64.(of_int32 x |> logand 0xFFFF_FFFF_L))
    | false -> return (Int Int32.(to_int x land 0xffff_ffff))
  in
  let value =
    match (arch, ty.Type.ty) with
    | __, Type.Bytes -> take ty.Type.size >>= fun x -> return (String x)
    | __, Type.String -> take ty.Type.size >>= fun x -> return (String x)
    | __, Type.Enum -> any_uint8 >>= fun x -> return (Enum x)
    | __, Type.Int (Signed, 8, FF) -> any_int8 >>= int ff
    | BE, Type.Int (Signed, 16, FF) -> BE.any_int16 >>= int ff
    | LE, Type.Int (Signed, 16, FF) -> LE.any_int16 >>= int ff
    | BE, Type.Int (Signed, 32, FF) -> BE.any_int32 >>= int32 ff'
    | LE, Type.Int (Signed, 32, FF) -> LE.any_int32 >>= int32 ff'
    | __, Type.Int (Unsigned, 8, ZZ) -> any_uint8 >>= uint8 0
    | LE, Type.Int (Unsigned, 16, ZZ) -> LE.any_uint16 >>= uint16 0
    | BE, Type.Int (Unsigned, 16, ZZ) -> BE.any_uint16 >>= uint16 0
    | LE, Type.Int (Unsigned, 32, ZZ) -> LE.any_int32 >>= uint32 0l
    | BE, Type.Int (Unsigned, 32, ZZ) -> BE.any_int32 >>= uint32 0l
    | __, Type.Int (Unsigned, 8, FF) -> any_uint8 >>= uint8 0xff
    | LE, Type.Int (Unsigned, 16, FF) -> LE.any_uint16 >>= uint16 0xffff
    | BE, Type.Int (Unsigned, 16, FF) -> BE.any_uint16 >>= uint16 0xffff
    | LE, Type.Int (Unsigned, 32, FF) -> LE.any_int32 >>= uint32 0xffff_ffff_l
    | BE, Type.Int (Unsigned, 32, FF) -> BE.any_int32 >>= uint32 0xffff_ffff_l
    | BE, Type.Float 32 -> BE.any_float >>= float
    | LE, Type.Float 32 -> LE.any_float >>= float
    | BE, Type.Float 64 -> BE.any_double >>= float
    | LE, Type.Float 64 -> LE.any_double >>= float
    | __, _ -> advance ty.Type.size *> return Unknown
  in

  let* before = pos in
  let* v = value in
  let* after = pos in
  let size = after - before in
  if size < ty.Type.size then
    (* This works around inconsistent files where the actual size does
       not match the expected size *)
    advance (ty.Type.size - size) >>= fun _ -> return v
  else return v

(** read a record (of type [ty]) of values. Each value in the record is read by
    [loop] which loops over the types of values we expect to find. Each record
    field is read by [base]. *)
let record arch ty =
  let cmp (x, _) (y, _) = Int.compare x y in
  let sort vs = List.sort cmp vs in
  let rec loop vs = function
    | [] -> return { msg = ty.Type.msg; fields = sort vs }
    | t :: ts -> base arch t >>= fun v -> loop ((t.Type.slot, v) :: vs) ts
  in
  loop [] ty.Type.fields >>= fun result ->
  (* skip over developer fields *)
  advance ty.Type.dev_fields *> return result

module File = struct
  let _dump dict =
    Dict.bindings dict
    |> List.rev_map (fun (k, v) -> (string_of_int k, Type.json v))
    |> fun x -> `Assoc x

  let header =
    any_int8 >>= function
    | (12 | 14) as size ->
        any_int8 >>= fun protocol ->
        LE.any_int16 >>= fun profile ->
        LE.any_int32 >>= fun length ->
        string ".FIT"
        *> (if size = 14 then advance 2 (* skip CRC *) else advance 0)
        *> return { protocol; profile; length = Int32.to_int length }
    | n -> fail "found unexpected header of size %d" n

  let block (dict, rs) =
    pos >>= fun p ->
    any_int8 >>= fun byte ->
    let key = byte & 0b0000_1111 in
    let tag = byte & 0b1111_0000 in
    match tag with
    | 0b0100_0000 ->
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* This is a block that defines a type - add it to the dict *)
        Type.record ~dev:false >>= fun d -> return (Dict.add key d dict, rs)
    | 0b0110_0000 ->
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* This is a block that defines a type - add it to the dict *)
        Type.record ~dev:true >>= fun d -> return (Dict.add key d dict, rs)
    | 0b0000_0000 -> (
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* This is a block holding values. Its shape is defined by the
           type it refers to and which we must have read earlier and
           should find in the dictionary *)
        match Dict.find_opt key dict with
        | Some ty ->
            let arch = ty.arch in
            record arch ty >>= fun r -> return (dict, r :: rs)
        | None ->
            pos >>= fun p ->
            fail "corrupted file? No type for key=%d offset=%d at %s" key p
              __LOC__)
    | _ when (tag & 0b1000_0000) <> 0 -> (
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* this is a compressed header for a value block that includes a
           timestamp. We ignore the timestamp and only read the other
           fields. *)
        let key = (tag & 0b0110_0000) >> 5 in
        match Dict.find_opt key dict with
        | Some ty ->
            let arch = ty.arch in
            record arch ty >>= fun r -> return (dict, r :: rs)
        | None ->
            pos >>= fun p ->
            fail "corrupted file? No type for key=%d offset=%d at %s" key p
              __LOC__)
    | n ->
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* _dump dict; *)
        fail "unexpected block with tag 0x%x at offset %d" n p

  let rec blocks xx finish =
    pos >>= fun p ->
    if p >= finish then return xx else block xx >>= fun xx -> blocks xx finish

  let read =
    let xx = (Dict.empty, []) in
    header >>= fun header ->
    pos >>= fun offset ->
    blocks xx (header.length + offset) >>= fun (_, records) ->
    return { header; records = List.rev records }
end

module MSG = struct
  (** Limited support for decoding records; the most common record is 20
      "record". *)

  let add map (k, v) = Dict.add k v map

  let msgs =
    [
      (0, "file_id")
    ; (1, "capabilities")
    ; (2, "device_settings")
    ; (3, "user_profile")
    ; (4, "hrm_profile")
    ; (5, "sdm_profile")
    ; (6, "bike_profile")
    ; (7, "zones_target")
    ; (8, "hr_zone")
    ; (9, "power_zone")
    ; (10, "met_zone")
    ; (12, "sport")
    ; (15, "goal")
    ; (18, "session")
    ; (19, "lap")
    ; (20, "record")
    ; (21, "event")
    ; (23, "device_info")
    ; (26, "workout")
    ; (27, "workout_step")
    ; (30, "weight_scale")
    ; (31, "course")
    ; (32, "course_point")
    ; (33, "totals")
    ; (34, "activity")
    ; (35, "software")
    ; (37, "file_capabilities")
    ; (38, "mesg_capabilities")
    ; (39, "field_capabilities")
    ; (49, "file_creator")
    ; (51, "blood_pressure")
    ]
    |> List.fold_left add Dict.empty

  let lookup key =
    match Dict.find_opt key msgs with
    | Some name -> name
    | None -> string_of_int key
end

module Decode = struct
  (* Values are scaled and need to be decoded. The scaling is depends on
     the record and slot of a value and what scaling is used by what
     slot is defined in the protocol and can't be deduced from the FIT
     file alone *)

  let timestamp v =
    let offset = 631065600.0 in
    match v with
    | Int n -> Int.to_float n +. offset
    | v -> failwith "%s: unexpected value: %s" __LOC__ (to_string v)

  let scale scale offset v =
    let scale = Float.of_int scale in
    let offset = Float.of_int offset in
    match v with
    | Int x -> (Float.of_int x /. scale) -. offset
    | Float x -> (x /. scale) -. offset
    | v -> failwith "%s: unexpected value: %s" __LOC__ (to_string v)

  let scale' scale offset v =
    match v with
    | Int x -> (x / scale) - offset
    | Float x -> Float.((x /. float scale) -. float offset |> round |> to_int)
    | v -> failwith "%s: unexpected value: %s" __LOC__ (to_string v)

  let latlon = function
    | Int x -> Int.to_float x *. 180.0 /. 2147483648.0
    | v -> failwith "%s: unexpected value: %s" __LOC__ (to_string v)
end

module JSON = struct
  let timestamp v =
    let offset = 631065600.0 in
    match v with
    | Int n ->
        let ts = Int.to_float n +. offset in
        let pt = Ptime.of_float_s ts in
        Option.fold pt ~none:`Null ~some:(fun x -> `String (Ptime.to_rfc3339 x))
    | _ -> `Null

  let scale scale offset v =
    try `Float (Decode.scale scale offset v) with _ -> `Null

  let latlon v = try `Float (Decode.latlon v) with _ -> `Null

  (* For the most common record "20", here is the meaning of slots and
     how to decode the associated value *)
  let value msg pos v =
    match (msg, pos, v) with
    | 20, 0, v -> ("latitude", latlon v)
    | 20, 1, v -> ("longitude", latlon v)
    | 20, 2, v -> ("altitude", scale 5 500 v)
    | 20, 3, v -> ("heartrate", scale 1 0 v)
    | 20, 4, v -> ("cadence", scale 1 0 v)
    | 20, 5, v -> ("distance", scale 100 0 v)
    | 20, 6, v -> ("speed", scale 1000 0 v)
    | 20, 7, v -> ("power", scale 1 0 v)
    | 20, 73, v -> ("enhanced_speed", scale 1000 0 v)
    | 20, 13, v -> ("temperature", scale 1 0 v)
    | 20, 12, v -> ("cycle_length", scale 100 0 v)
    | 20, 19, v -> ("total_cycles", scale 1 0 v)
    | _, 253, v -> ("timestamp", timestamp v)
    | _, _, Enum n -> (string_of_int pos, `Int n)
    | _, _, String s -> (string_of_int pos, `String s)
    | _, _, Int i -> (string_of_int pos, `Int i)
    | _, _, Int32 i -> (string_of_int pos, `Float (Int32.to_float i))
    | _, _, Int64 i -> (string_of_int pos, `Float (Int64.to_float i))
    | _, _, Float f when Float.is_nan f -> (string_of_int pos, `Null)
    | _, _, Float f -> (string_of_int pos, `Float f)
    | _, _, Unknown -> (string_of_int pos, `Null)

  let field msg (pos, v) = value msg pos v

  let record r =
    `Assoc
      (("msg", `String (MSG.lookup r.msg)) :: List.map (field r.msg) r.fields)
end

(** if decoding fails, we record the field as not present *)
let get slot fields decoder =
  List.assoc_opt slot fields |> function
  | Some x -> ( try Some (decoder x) with _ -> None)
  | None -> None

module Record = struct
  (** The messages with tag 20 (called "record") are at the heart of all FIT
      files as they contain the measurements. These records may contain
      different values and their presence cannot be expected. This module
      provides a representation for such records but covers only the most common
      values and is not comprehensive *)

  type t = {
      latitude : float option
    ; longitude : float option
    ; timestamp : float option
    ; altitude : float option
    ; heartrate : float option
    ; cadence : float option
    ; power : float option
    ; speed : float option
    ; distance : float option
    ; temperature : float option
    ; cycle_length : float option
    ; total_cycles : float option
  }

  let record = function
    | { msg = 20; fields } -> (
        try
          Some
            {
              latitude = get 0 fields Decode.latlon
            ; longitude = get 1 fields Decode.latlon
            ; timestamp = get 253 fields Decode.timestamp
            ; altitude = get 2 fields (Decode.scale 5 500)
            ; heartrate = get 3 fields (Decode.scale 1 0)
            ; cadence = get 4 fields (Decode.scale 1 0)
            ; distance = get 5 fields (Decode.scale 100 0)
            ; power = get 7 fields (Decode.scale 1 0)
            ; temperature = get 13 fields (Decode.scale 1 0)
            ; speed = get 6 fields (Decode.scale 1000 0)
            ; cycle_length = get 12 fields (Decode.scale 100 0)
            ; total_cycles = get 19 fields (Decode.scale 1 0)
            }
        with _ -> None)
    | _ -> None
end

module Device = struct
  type t = { serial : int option; manufacturer : int option }

  let null = { serial = None; manufacturer = None }

  let _manufacturer = function
    | 1 -> "garmin"
    | 2 -> "garmin_fr405_antfs"
    | 3 -> "zephyr"
    | 4 -> "dayton"
    | 5 -> "idt"
    | 6 -> "srm"
    | 7 -> "quarq"
    | 8 -> "ibike"
    | 9 -> "saris"
    | 10 -> "spark_hk"
    | 11 -> "tanita"
    | 12 -> "echowell"
    | 13 -> "dynastream_oem"
    | 14 -> "nautilus"
    | 15 -> "dynastream"
    | 16 -> "timex"
    | 17 -> "metrigear"
    | 18 -> "xelic"
    | 19 -> "beurer"
    | 20 -> "cardiosport"
    | 21 -> "a_and_d"
    | 22 -> "hmm"
    | 23 -> "suunto"
    | 24 -> "thita_elektronik"
    | 25 -> "gpulse"
    | 26 -> "clean_mobile"
    | 27 -> "pedal_brain"
    | 28 -> "peaksware"
    | 29 -> "saxonar"
    | 30 -> "lemond_fitness"
    | 31 -> "dexcom"
    | 32 -> "wahoo_fitness"
    | 33 -> "octane_fitness"
    | 34 -> "archinoetics"
    | 35 -> "the_hurt_box"
    | 36 -> "citizen_systems"
    | 37 -> "magellan"
    | 38 -> "osynce"
    | 39 -> "holux"
    | 40 -> "concept2"
    | 42 -> "one_giant_leap"
    | 43 -> "ace_sensor"
    | 44 -> "brim_brothers"
    | 45 -> "xplova"
    | 46 -> "perception_digital"
    | 47 -> "bf1systems"
    | 48 -> "pioneer"
    | 49 -> "spantec"
    | 50 -> "metalogics"
    | 51 -> "4iiiis"
    | 52 -> "seiko_epson"
    | 53 -> "seiko_epson_oem"
    | 54 -> "ifor_powell"
    | 55 -> "maxwell_guider"
    | 56 -> "star_trac"
    | 57 -> "breakaway"
    | 58 -> "alatech_technology_ltd"
    | 59 -> "mio_technology_europe"
    | 60 -> "rotor"
    | 61 -> "geonaute"
    | 62 -> "id_bike"
    | 63 -> "specialized"
    | 64 -> "wtek"
    | 65 -> "physical_enterprises"
    | 66 -> "north_pole_engineering"
    | 67 -> "bkool"
    | 68 -> "cateye"
    | 69 -> "stages_cycling"
    | 70 -> "sigmasport"
    | 71 -> "tomtom"
    | 72 -> "peripedal"
    | 73 -> "wattbike"
    | 76 -> "moxy"
    | 77 -> "ciclosport"
    | 78 -> "powerbahn"
    | 79 -> "acorn_projects_aps"
    | 80 -> "lifebeam"
    | 81 -> "bontrager"
    | 82 -> "wellgo"
    | 83 -> "scosche"
    | 84 -> "magura"
    | 85 -> "woodway"
    | 86 -> "elite"
    | 87 -> "nielsen_kellerman"
    | 88 -> "dk_city"
    | 89 -> "tacx"
    | 90 -> "direction_technology"
    | 91 -> "magtonic"
    | 92 -> "1partcarbon"
    | 93 -> "inside_ride_technologies"
    | 94 -> "sound_of_motion"
    | 95 -> "stryd"
    | 96 -> "icg"
    | 97 -> "MiPulse"
    | 98 -> "bsx_athletics"
    | 99 -> "look"
    | 100 -> "campagnolo_srl"
    | 101 -> "body_bike_smart"
    | 102 -> "praxisworks"
    | 103 -> "limits_technology"
    | 104 -> "topaction_technology"
    | 105 -> "cosinuss"
    | 255 -> "development"
    | 257 -> "healthandlife"
    | 258 -> "lezyne"
    | 259 -> "scribe_labs"
    | 260 -> "zwift"
    | 261 -> "watteam"
    | 262 -> "recon"
    | 263 -> "favero_electronics"
    | 264 -> "dynovelo"
    | 265 -> "strava"
    | 266 -> "precor"
    | 267 -> "bryton"
    | 268 -> "sram"
    | 269 -> "navman"
    | 270 -> "cobi"
    | 271 -> "spivi"
    | 272 -> "mio_magellan"
    | 273 -> "evesports"
    | n -> string_of_int n

  let device fit =
    List.find_opt (function { msg = 23; _ } -> true | _ -> false) fit.records
    |> function
    | Some { msg = 23; fields } -> (
        try
          {
            serial = get 3 fields (Decode.scale' 1 0)
          ; manufacturer = get 2 fields (Decode.scale' 1 0)
          }
        with _ -> null)
    | _ -> null
end

let to_json fit = `List (List.rev_map JSON.record fit.records |> List.rev)

let records fit =
  fit.records |> List.rev_map Record.record |> List.filter Option.is_some
  |> List.rev_map Option.get

let device fit = Device.device fit

let header str =
  let consume = Consume.Prefix in
  parse_string ~consume File.header str

(** parse a string as a FIT file *)
let parse str =
  let consume = Consume.Prefix in
  parse_string ~consume File.read str

let read_file ~max_size path =
  let ic = open_in path in
  defer (fun () -> close_in ic) @@ fun () ->
  let size = in_channel_length ic in
  if size > max_size then failwith "input file %s exceeds maximum size" path
  else really_input_string ic size

let read ?(max_size = 100 * 1024) path =
  let error fmt = Printf.ksprintf (fun str -> Error str) fmt in
  try read_file ~max_size path |> parse
  with e -> error "Can't process %s: %s" path (Printexc.to_string e)
