open Rresult
module C = Cmdliner
module J = Yojson
module F = Fit
module P = Ptime

let cmd = "fit2srt"

let build =
  Printf.sprintf "Commit: %s Built on: %s" Build.git_revision Build.build_time

let round x = floor (x +. 0.5)

let round_dfrac d x =
  if x -. round x = 0. then x
  else
    (* x is an integer. *)
    let m = 10. ** float d in
    (* m moves 10^-d to 1. *)
    floor ((x *. m) +. 0.5) /. m

let _round0 = round
let _round1 = round_dfrac 1
let _round2 = round_dfrac 2

let split500 v =
  let time = int_of_float ((500.0 /. v) +. 0.5) in
  (* round to nearest sec *)
  let min = time / 60 in
  let sec = time mod 60 in
  Printf.sprintf "%d:%02d" min sec

module Span = struct
  (* A span is a time span in seconds *)

  let to_string t =
    let int = Float.to_int in
    let hh = t /. 3600. |> modf |> snd in
    let mm = (t -. (hh *. 3600.)) /. 60. |> modf |> snd in
    let ss = t -. (hh *. 3600.) -. (mm *. 60.) in
    Printf.sprintf "%02d:%02d:%05.2f" (int hh) (int mm) ss
end

let iter2 n xs f =
  let rec loop n xs =
    match xs with
    | [] -> ()
    | [ _ ] -> ()
    | x :: (y :: _ as ys) ->
        f n x y;
        loop (n + 1) ys
  in
  loop n xs

let records path =
  Fit.read ~max_size:(1024 * 512) path
  |> R.reword_error (fun str -> `Msg str)
  |> R.failwith_error_msg |> Fit.records |> List.rev

let select ts duration records =
  records
  |> List.filter @@ function
     | Fit.Record.
         {
           timestamp = Some ts'
         ; latitude = Some _
         ; longitude = Some _
         ; speed = Some _
         ; cadence = Some _
         ; distance = Some _
         ; _
         }
       when ts' > ts && ts' <= ts +. duration ->
         true
     | _ -> false

let srt ~n ~t_start ~t_end ~spm ~v ~dps =
  Printf.printf {|
%d
%s --> %s
%.1f %s/500 %.1fm
|} n (Span.to_string t_start)
    (Span.to_string t_end) spm (split500 v) dps

let json = function
  | Fit.Record.
      { timestamp = Some ts; latitude = Some lat; longitude = Some lon; _ } ->
      `Assoc [ ("ts", `Float ts); ("lat", `Float lat); ("lon", `Float lon) ]
  | _ -> `Null

let _fit duration ts path =
  records path |> select ts (float duration) |> List.map json |> fun json ->
  `List json |> J.pretty_to_channel stdout

(** Distance per stroke in SpeedCoach data: data points are aligned to full
    seconds. But a stroke may have ended earlier. The cycle length is the
    distance travled since the end of the stroke to the full seconds. So the
    distance per stroke is the distance between two data point points, minus the
    cycle length. *)

let fit duration ts path =
  let default none x = Option.fold ~none ~some:Fun.id x in
  let rs = records path |> select ts (float duration) in
  iter2 1 rs @@ fun n x y ->
  let t_start = Option.get x.Fit.Record.timestamp -. ts in
  let t_end = Option.get y.Fit.Record.timestamp -. ts in
  let v = Option.get y.Fit.Record.speed in
  let spm = Option.get y.Fit.Record.cadence in
  let d_start = Option.get x.Fit.Record.distance in
  let d_end = Option.get y.Fit.Record.distance in
  let cycle_length = default 0.0 y.Fit.Record.cycle_length in
  let dps = d_end -. d_start -. cycle_length in
  srt ~n ~t_start ~t_end ~spm ~v ~dps

module Command = struct
  let help =
    [
      `P {|Read a binary FIT file and emit data as srt video sub titles.|}
    ; `P
        {|$(mname) is a tool to embed FIT data as produced by a personal
        fitness device as sub titles into a video. The goal is to
        synchronise the two: given a video with a known precise starting
        time, emit the corresponding FIT data in srt format to stdout.
        The actual embedding is left to tools like ffmpeg(1).|}
    ; `S "MORE HELP"
    ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
    ; `S "BUGS"
    ; `P "Check bug reports at https://github.com/lindig/fit/issues"
    ; `S "SEE ALSO"
    ; `P "ffmpeg(1)"
    ; `S "BUILD DETAILS"
    ; `P build
    ]

  let msg fmt = Printf.ksprintf (fun str -> `Msg str) fmt

  let rfc3339 =
    let parse str =
      match Ptime.of_rfc3339 str with
      | Ok (t, _, _) -> R.ok (Ptime.to_float_s t)
      | _ -> R.error (msg "Can't parse %s as date-time" str)
    in
    let print ppf ts =
      let t = Ptime.of_float_s ts |> Option.get in
      Format.fprintf ppf "%s" (Ptime.to_rfc3339 t)
    in
    C.Arg.conv ~docv:"TS" (parse, print)

  let mmss =
    let parse str =
      try R.ok (Scanf.sscanf str "%u:%u" (fun min sec -> (min * 60) + sec))
      with _ -> R.error (msg "Can't parse %s as min:sec value" str)
    in
    let print ppf sec = Format.fprintf ppf "%d:%0d" (sec / 60) (sec mod 60) in
    C.Arg.conv ~docv:"S" (parse, print)

  let duration =
    C.Arg.(
      value & opt mmss 30
      & info [ "d"; "duration" ] ~docv:"DURATION"
          ~doc:"Duration in min:sec of video (and data to extract)")

  let timestamp =
    let now = Unix.time () in
    let doc = "Timestamp for start of video." in
    C.Arg.(value & pos 0 rfc3339 now & info [] ~docv:"timestamp" ~doc)

  let path =
    C.Arg.(
      value & pos 1 file "file.fit"
      & info [] ~docv:"file.fit" ~doc:"FIT file to process.")

  let srt =
    let doc = "emit FIT data in srt format for video sub titles" in
    let info = C.Cmd.info cmd ~doc ~man:help in
    C.(Cmd.v info Term.(const fit $ duration $ timestamp $ path))
end

let main () = C.Cmd.eval Command.srt |> exit
let () = if !Sys.interactive then () else main ()
