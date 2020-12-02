open Rresult
module C = Cmdliner
module J = Ezjsonm
module F = Fit

let fit name =
  Fit.read name |> R.get_ok |> Fit.to_json |> J.to_channel ~minify:false stdout

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
    ]

  let path =
    C.Arg.(
      value & pos 0 file "file.fit"
      & info [] ~docv:"file.fit" ~doc:"FIT file to process.")

  let fit =
    let doc = "process FIT file" in
    C.Term.(const fit $ path, info "fit" ~doc ~man:help)
end

let main () = C.Term.(exit @@ eval Command.fit)

let () = if !Sys.interactive then () else main ()