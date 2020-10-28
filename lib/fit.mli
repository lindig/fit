(* A parser for FIT files *)

type header = {
    protocol : int
  ; profile : int
  ; length : int  (** size of data blocks in file *)
}

(** A [value] is part of a data record. The interpretation of the value
    depends on the record and there is no obvious interpretation for a
    value by itself *)
type value =
  | Enum of int
  | String of string
  | Int of int
  | Int32 of int32
  | Float of float
  | Unknown

type record = { msg : int; fields : (int * value) list }
(** A [record] holds a set of values. The purpose of the record is
    implied by its [msg] member. Each [field] in a record has a value
    and an [int] position. The combination of [msg] and position
    conveys the interpretation of that value and it is defined by the
    FIT protocol. At this level, no interpretation is provided *)

type t = { header : header; records : record list }
(** A FIT file is a header and a list of records (in reversed order) *)

val read : string -> (t, string) result
(** [read path] reads a FIT file from [path] in the file system. *)

val to_json : t -> Ezjsonm.t
