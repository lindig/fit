open Angstrom

let defer f = Fun.protect ~finally:f

(** redefine & as bitwise operation, && is still logical and *)
let ( & ) = Int.logand

let ( >> ) = Int.shift_right

let fail_with fmt = Printf.kprintf (fun msg -> fail msg) fmt

type arch = BE  (** Big Endian *) | LE  (** Little Endian *)

module Dict = Map.Make (Int)

module Type = struct
  type sign = Signed | Unsigned

  type base = Enum | Bytes | String | Int of sign * int | Float of int

  type field = {
      slot : int  (** position within record - defines purpose *)
    ; size : int  (** in bytes *)
    ; ty : base  (** representation *)
  }

  type record = { msg : int; arch : arch; fields : field list }

  (** parse a [field] definition from a FIT file *)
  let field =
    any_uint8 >>= fun slot ->
    any_uint8 >>= fun size ->
    any_uint8 >>= fun ty' ->
    let ty =
      match ty' & 0b1111 with
      | 0      -> Enum
      | 1      -> Int (Signed, 8)
      | 2 | 10 -> Int (Unsigned, 8)
      | 3      -> Int (Signed, 16)
      | 4 | 11 -> Int (Unsigned, 16)
      | 5      -> Int (Signed, 32)
      | 6 | 12 -> Int (Unsigned, 32)
      | 7      -> String
      | 8      -> Float 32
      | 9      -> Float 64
      | 13     -> Bytes
      | _      -> failwith "unknown field base type"
    in
    return { slot; size; ty }

  let record =
    (int8 0 *> any_int8 >>= function
     | 0 -> return LE
     | 1 -> return BE
     | n -> fail_with "expected 0 or 1 in byte for endianness, found %d" n)
    >>= fun arch ->
    (match arch with LE -> LE.any_uint16 | BE -> BE.any_uint16) >>= fun msg ->
    any_int8 >>= fun n ->
    count n field >>= fun fields -> return { msg; arch; fields }
end

type header = { protocol : int; profile : int; length : int }

type value =
  | Enum of int
  | String of string
  | Int of int
  | Int32 of int32
  | Float of float
  | Unknown

type record = { msg : int; fields : (int * value) list }

type t = { header : header; records : record list }

let base arch ty =
  match (arch, ty.Type.ty) with
  | _, Type.Enum -> any_uint8 >>= fun x -> return @@ Enum x
  | _, Type.Bytes -> take ty.Type.size >>= fun x -> return @@ String x
  | _, Type.String -> take ty.Type.size >>= fun x -> return @@ String x
  | _, Type.Int (Signed, 8) -> any_int8 >>= fun x -> return @@ Int x
  | BE, Type.Int (Signed, 16) -> BE.any_int16 >>= fun x -> return @@ Int x
  | LE, Type.Int (Signed, 16) -> LE.any_int16 >>= fun x -> return @@ Int x
  | BE, Type.Int (Signed, 32) -> BE.any_int32 >>= fun x -> return @@ Int32 x
  | LE, Type.Int (Signed, 32) -> LE.any_int32 >>= fun x -> return @@ Int32 x
  | _, Type.Int (Unsigned, 8) -> any_uint8 >>= fun x -> return @@ Int x
  | LE, Type.Int (Unsigned, 16) -> LE.any_uint16 >>= fun x -> return @@ Int x
  | BE, Type.Int (Unsigned, 16) -> BE.any_uint16 >>= fun x -> return @@ Int x
  | LE, Type.Int (Unsigned, 32) -> LE.any_int32 >>= fun x -> return @@ Int32 x
  | BE, Type.Int (Unsigned, 32) -> BE.any_int32 >>= fun x -> return @@ Int32 x
  | BE, Type.Float 32 -> BE.any_float >>= fun x -> return @@ Float x
  | LE, Type.Float 32 -> LE.any_float >>= fun x -> return @@ Float x
  | BE, Type.Float 64 -> BE.any_double >>= fun x -> return @@ Float x
  | LE, Type.Float 64 -> LE.any_double >>= fun x -> return @@ Float x
  | _, _ -> advance ty.Type.size *> return Unknown

let record arch ty =
  let rec loop vs = function
    | []      -> return { msg = ty.Type.msg; fields = List.rev vs }
    | t :: ts -> base arch t >>= fun v -> loop ((t.Type.slot, v) :: vs) ts
  in
  loop [] ty.Type.fields

module File = struct
  let header =
    any_int8 >>= function
    | (12 | 14) as size ->
        any_int8 >>= fun protocol ->
        LE.any_int16 >>= fun profile ->
        LE.any_int32 >>= fun length ->
        string ".FIT"
        *> (if size = 14 then advance 2 (* skip CRC *) else advance 0)
        *> return { protocol; profile; length = Int32.to_int length }
    | n                 -> fail_with "found unexpected header of size %d" n

  let block (dict, rs) =
    any_int8 >>= fun byte ->
    let key = byte & 0b0000_1111 in
    let tag = byte & 0b1111_0000 in
    match tag with
    | 0b0100_0000 ->
        (* This is a block that defines a type - add it to the dict *)
        Type.record >>= fun d -> return (Dict.add key d dict, rs)
    | 0b0000_0000 -> (
        (* This is a block holding values. Its shape is defined by the
           type it refers to and which we must have read earlier and
           should find in the dictionary *)
        match Dict.find_opt key dict with
        | Some ty ->
            let arch = ty.arch in
            record arch ty >>= fun r -> return (dict, r :: rs)
        | None    -> fail_with "corrupted file? can't find type for key %d" key
        )
    | _ when (tag & 0b1000_0000) <> 0 -> (
        (* this is a compressed header for a value block that includes a
           timestamp. We ignore the timestamp and only read the other
           fields. *)
        let key = (tag & 0b0110_0000) >> 5 in
        match Dict.find_opt key dict with
        | Some ty ->
            let arch = ty.arch in
            record arch ty >>= fun r -> return (dict, r :: rs)
        | None    -> fail_with "corrupted file? Can't find type for key %d" key
        )
    | n -> fail_with "unexpected block with tag %x" n

  let rec blocks xx finish =
    pos >>= fun p ->
    if p >= finish then return xx else block xx >>= fun xx -> blocks xx finish

  let read =
    let xx = (Dict.empty, []) in
    header >>= fun header ->
    pos >>= fun offset ->
    blocks xx (header.length + offset) >>= fun (_, records) ->
    return { header; records }
end

module JSON = struct
  let value = function
    | Enum n    -> `Float (Float.of_int n)
    | String s  -> `String s
    | Int i     -> `Float (Float.of_int i)
    | Int32 i32 -> `Float (Int32.to_float i32)
    | Float f   -> `Float f
    | Unknown   -> `Null

  let field (pos, v) = (string_of_int pos, value v)

  let record r =
    `O (("msg", `String (string_of_int r.msg)) :: List.map field r.fields)
end

let to_json fit = `A (List.rev_map JSON.record fit.records)

let read_file path =
  let ic = open_in path in
  defer (fun () -> close_in ic) @@ fun () ->
  really_input_string ic (in_channel_length ic)

let read path =
  let consume = Consume.Prefix in
  try read_file path |> parse_string ~consume File.read
  with e ->
    Error (Printf.sprintf "Can't process %s: %s" path (Printexc.to_string e))
