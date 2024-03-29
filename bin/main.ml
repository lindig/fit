open Rresult
module C = Cmdliner
module J = Yojson
module F = Fit

let build =
  Printf.sprintf "Commit: %s Built on: %s" Build.git_revision Build.build_time

let fit name =
  Fit.read ~max_size:(1024 * 512) name
  |> R.reword_error (fun str -> `Msg str)
  |> R.failwith_error_msg |> Fit.to_json |> J.pretty_to_channel stdout

module Command = struct
  let help =
    [
      `P {|Read a binary FIT file and emit it as JSON to stdout.|}
    ; `P
        {|Values
           in FIT files are often scaled. Currently no scaling is
           implemented and the values are reported raw as they are read
           from the binary. The meaning of fields in a record depend 
           on the message number of the record, which is defined by 
           the FIT protocol. Currently the $(mname) command has no knowledge
           about these.  |}
    ; `S "MORE HELP"
    ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
    ; `S "BUGS"
    ; `P "Check bug reports at https://github.com/lindig/fit/issues"
    ; `S "BUILD DETAILS"
    ; `P build
    ]

  let path =
    C.Arg.(
      value & pos 0 file "file.fit"
      & info [] ~docv:"file.fit" ~doc:"FIT file to process.")

  let fit =
    let doc = "process FIT file" in
    let info = C.Cmd.info "fit" ~doc ~man:help in
    C.(Cmd.v info Term.(const fit $ path))
end

let main () = C.Cmd.eval Command.fit |> exit
let () = if !Sys.interactive then () else main ()
