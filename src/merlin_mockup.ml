(** [run] = New_merlin.run ou New_commands.run *)
let run =
  let req_count = ref 0 in
  fun config hermes ->
    Hermes.send_and_wait hermes (Motyper.Msg `Cancel);
    incr req_count;
    let result = Mopipeline.get config hermes in
    Utils.log 0 "Request nb %i - Beginning analysis" !req_count;
    Option.iter (fun r -> Moquery_commands.analysis hermes r config) result

(** [main] = Ocaml_merlin_server.main *)
let main () =
  let hermes = Hermes.create () in
  Utils.log 0 "Spawning typer";

  let domain_typer = Domain.spawn (fun () -> Mopipeline.domain_typer hermes) in
  Server.listen ~handle:(fun req ->
      run req hermes;
      let res = !Motyper.res |> List.rev in
      Moparser.to_string (ref res));
  Hermes.send_and_wait hermes (Motyper.Msg `Close);
  Domain.join domain_typer

let () = main ()
