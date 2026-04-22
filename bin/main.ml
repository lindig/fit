module C = Cmdliner
module J = Yojson
module F = Fit

let build =
  Printf.sprintf "Commit: %s Built on: %s" Build.git_revision Build.build_time

let ( let* ) = Result.bind

let map f xs =
  let rec loop acc = function
    | x :: xs -> (
        match f x with Ok r -> loop (r :: acc) xs | Error _ as err -> err)
    | [] -> Ok (List.rev acc)
  in
  loop [] xs

let process name =
  let* fit = Fit.read ~max_size:(1024 * 512) name in
  let json = `Assoc [ ("file", `String name); ("fit", Fit.to_json fit) ] in
  Ok json

let fit names =
  let* json = map process names in
  J.pretty_to_channel stdout (`List json);
  Ok ()

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

  let paths =
    C.Arg.(
      value & pos_all file []
      & info [] ~docv:"file.fit" ~doc:"Files to analyse in FIT format")

  let fit =
    let doc = "process FIT files" in
    let info = C.Cmd.info "fit" ~doc ~man:help in
    C.(Cmd.v info Term.(const fit $ paths))
end

let main () = C.Cmd.eval_result Command.fit |> exit
let () = if !Sys.interactive then () else main ()
