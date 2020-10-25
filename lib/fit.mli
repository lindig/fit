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

val read : string -> (t, string) result
