module A = Angstrom

let ( >>= ) = A.( >>= )
let ff = String.init 10 (fun _ -> Char.chr 0xff)
let ee = String.init 10 (fun _ -> Char.chr 0xee)
let consume = A.Consume.Prefix
let int8 = A.any_int8
let uint8 = A.any_uint8
let int16 = A.BE.any_int16
let uint16 = A.BE.any_uint16
let int32 = A.BE.any_int32 >>= fun i -> Int32.to_int i |> A.return

let uint32 =
  A.BE.any_int32 >>= fun i -> Int32.to_int i land 0xffff_ffff |> A.return

let parse p str =
  A.parse_string ~consume p str |> Result.get_ok |> fun n ->
  Printf.sprintf "%d %x\n" n n
