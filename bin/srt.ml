open Rresult
module C = Cmdliner
module J = Yojson
module F = Fit
module P = Ptime

let cmd = "fit2srt"

let build =
  Printf.sprintf "Commit: %s Built on: %s" Build.git_revision Build.build_time

let rec iter2 f xs =
  match xs with
  | [] -> ()
  | [ _ ] -> ()
  | x :: (y :: _ as ys) ->
      f x y;
      iter2 f ys

let _iter2 = iter2

let records path =
  Fit.read ~max_size:(1024 * 512) path
  |> R.reword_error (fun str -> `Msg str)
  |> R.failwith_error_msg |> Fit.records |> List.rev

let select ts duration records =
  records
  |> List.filter @@ function
     | Fit.Record.
         { timestamp = Some ts'; latitude = Some _; longitude = Some _; _ }
       when ts' > ts && ts' <= ts +. duration ->
         true
     | _ -> false

let json = function
  | Fit.Record.
      { timestamp = Some ts; latitude = Some lat; longitude = Some lon; _ } ->
      `Assoc [ ("ts", `Float ts); ("lat", `Float lat); ("lon", `Float lon) ]
  | _ -> `Null

let fit duration ts path =
  records path |> select ts (float duration) |> List.map json |> fun json ->
  `List json |> J.pretty_to_channel stdout

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
