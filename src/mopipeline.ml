open Motyper

type t = {
  source : string;
  parsedtree : Motyper.parsedtree;
  result : Motyper.result;
}

let process config hermes =
  let parsedtree = Moparser.parse config.Moconfig.source in
  Effect.Deep.match_with
    (fun () -> Motyper.run config hermes parsedtree)
    ()
    {
      retc = Fun.id;
      exnc = raise;
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Motyper.Partial (Run evals) ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Utils.log 1 "Sharing partial result";
                  Hermes.send_and_wait hermes (Partial evals);
                  Utils.log 1 "Shared partial result!";
                  Effect.Deep.continue k ())
          | _ -> None);
    }

let get config hermes =
  Hermes.send_and_wait hermes (Config config);
  Utils.log 0 "Config changed and received";
  match Hermes.recv_clear hermes with
  | Partial pipeline ->
      Utils.log 0 "Got partial result";
      let pipeline =
        { source = config.source; parsedtree = []; result = pipeline }
      in
      Some pipeline
  | Msg (`Exn exn) ->
      Utils.log 0 "Got exception: %s" (Printexc.to_string exn);
      raise exn
  | _ -> failwith "Unexpected message"

let make config shared = process config shared

(** [domain_typer] *)
let domain_typer hermes =
  let rec loop () =
    Utils.log 1 "Looping";
    try
      match Hermes.recv_clear hermes with
      | Msg `Close ->
          Utils.log 0 "Closing";
          (* Stopping ! *)
          ()
      | Msg `Cancel ->
          Utils.log 0 "Cancelling";
          loop ()
      | Config config ->
          Utils.log 0 "Beginning new config";
          let pipeline = make config hermes in
          (match config.completion with
          | All -> Hermes.send_and_wait hermes (Partial pipeline)
          | _ -> (* Already shared *) ());
          loop ()
      | _ -> failwith "unexpected message in domain_typer"
    with
    | Cancel_or_closing ->
        Utils.log 0 "Caught Cancel_or_closing.";
        loop ()
    | Exception_after_partial exn ->
        let exn = Printexc.to_string exn in
        Utils.log 0 "Caught an exception after partial result : %s." exn;
        loop ()
    | exn ->
        Hermes.send_and_wait hermes (Msg (`Exn exn));
        loop ()
  in
  loop ()
