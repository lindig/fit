(* A parser for FIT files *)

type header = {
    protocol : int
  ; profile : int
  ; length : int  (** size of data blocks in file *)
}

(** A [value] is part of a data record. The interpretation of the value depends
    on the record and there is no obvious interpretation for a value by itself
*)
type value =
  | Enum of int
  | String of string
  | Int of int
  | Float of float
  | Unknown

type record = { msg : int; fields : (int * value) list }
(** A [record] holds a set of values. The purpose of the record is implied by
    its [msg] member. Each [field] in a record has a value and an [int]
    position. The combination of [msg] and position conveys the interpretation
    of that value and it is defined by the FIT protocol. At this level, no
    interpretation is provided *)

type t = { header : header; records : record list }
(** A FIT file has a header and a list of records (in reversed order) *)

val parse : string -> (t, string) result
(** [parse fit] parses the binary content [fit], typically loaded from a file *)

val header : string -> (header, string) result
(** [header fit] parse just the header of a [fit] stream *)

val read_file : max_size:int -> string -> string
(** [read_file path] reads a file into a string. The file must not exceed
    [max_size]*)

val read : ?max_size:int -> string -> (t, string) result
(** [read path] reads a FIT file from [path] in the file system. The input file
    must not exceed [max_size] (100kb by default) to protect against attacks
    when reading user-provided files *)

val to_json : t -> Yojson.t
(** represent FIT file as JSON. This includes decoding some common fields in
    "record" messages. *)

module Decode : sig
  (** [value]s reported as part of a [record] are often encoded or scaled. This
      module provides typical decoders. How values are decoded depends on the
      field and the information is not part of the FIT file per se but is part
      of the format definition. *)

  val timestamp : value -> float
  (** decode a time-stamp field; it expects an [Int] value *)

  val scale : int -> int -> value -> float
  (** many values are scaled. The scale consists of an offset and a factor:
      [scale factor offset value] decodes [value] using [factor] and [offset].
      The [value] is expected to be an [Int] or [Float] value. *)

  val latlon : value -> float
  (** Coordinates in latitude or longitude are decoded using [latlon]. The
      [value] is expected to be an [Int]. *)
end

module Record : sig
  (** decode "record" messages *)

  type t = {
      latitude : float option
    ; longitude : float option
    ; timestamp : float option
    ; altitude : float option
    ; heartrate : float option
    ; cadence : float option
    ; speed : float option
    ; distance : float option
    ; temperature : float option
    ; cycle_length : float option
  }
end

val records : t -> Record.t list
(** Extract and decode common fields *)
