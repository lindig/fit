open Rresult
module C = Cmdliner
module J = Yojson
module F = Fit
module P = Ptime

let cmd = "fit2srt"

let build =
  Printf.sprintf "Commit: %s Built on: %s" Build.git_revision Build.build_time

let fit _duration _ts name =
  Fit.read ~max_size:(1024 * 512) name
  |> R.reword_error (fun str -> `Msg str)
  |> R.failwith_error_msg |> Fit.to_json |> J.pretty_to_channel stdout

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

  let duration =
    C.Arg.(
      value & opt float 30.0
      & info [ "d"; "duration" ] ~docv:"DURATION"
          ~doc:"Duration in seconds of video (and data to extract)")

  let timestamp =
    let now : P.t = Unix.time () |> Ptime.of_float_s |> Option.get in
    let rfc3339 : string = Ptime.to_rfc3339 now in
    let doc = "Timestamp for start of video." in
    C.Arg.(value & pos 0 string rfc3339 & info [] ~docv:"timestamp" ~doc)

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
