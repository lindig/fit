module C = Cmdliner
module J = Yojson
module F = Fit

let record_to_json (r : F.Record.t) =
  let open F.Record in
  `Assoc
    [ ("timestamp", match r.timestamp with Some ts -> `Float ts | None -> `Null)
    ; ("latitude", match r.latitude with Some v -> `Float v | None -> `Null)
    ; ("longitude", match r.longitude with Some v -> `Float v | None -> `Null)
    ; ("altitude", match r.altitude with Some v -> `Float v | None -> `Null)
    ; ("heartrate", match r.heartrate with Some v -> `Float v | None -> `Null)
    ; ("cadence", match r.cadence with Some v -> `Float v | None -> `Null)
    ; ("power", match r.power with Some v -> `Float v | None -> `Null)
    ; ("speed", match r.speed with Some v -> `Float v | None -> `Null)
    ; ("distance", match r.distance with Some v -> `Float v | None -> `Null)
    ; ("temperature", match r.temperature with Some v -> `Float v | None -> `Null)
    ; ("cycle_length", match r.cycle_length with Some v -> `Float v | None -> `Null)
    ; ("total_cycles", match r.total_cycles with Some v -> `Float v | None -> `Null)
    ]

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

let process records name =
  let* fit = Fit.read ~max_size:(1024 * 512) name in
  let json =
    if records then
      `Assoc
        [ ("file", `String name)
        ; ("records", `List (List.map record_to_json (List.rev (F.records ~join:true fit))))
        ]
    else
      `Assoc [ ("file", `String name); ("fit", Fit.to_json fit) ]
  in
  Ok json

let fit records debug names =
  Fit.debug := debug;
  let* json = map (process records) names in
  J.pretty_to_channel stdout (`List json);
  Ok ()

module Command = struct
  let help =
    [
      `P {|Read a binary FIT file and emit it as JSON to stdout.|}
    ; `P
        {|When --records is given, only the decoded Record.t values are emitted
           (using Fit.records ~join:true). This is useful to test the join logic
           without the full FIT structure and other messages.|}
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

  let debug =
    let doc = {|Enable debugging output|} in
    C.Arg.(value & flag & info [ "debug" ] ~docv:"DEBUG" ~doc)

  let records =
    let doc = {|Emit only Record.t values (using Fit.records ~join:true). Useful for testing the join logic.|} in
    C.Arg.(value & flag & info [ "records" ] ~docv:"RECORDS" ~doc)

  let paths =
    C.Arg.(
      value & pos_all file []
      & info [] ~docv:"file.fit" ~doc:"Files to analyse in FIT format")

  let fit =
    let doc = "process FIT files" in
    let info = C.Cmd.info "fit" ~doc ~man:help in
    C.(Cmd.v info Term.(const fit $ records $ debug $ paths))
end

let main () = C.Cmd.eval_result Command.fit |> exit
let () = if !Sys.interactive then () else main ()
