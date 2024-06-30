let main () =
  let counter = Imp_batched_counter.init ~ctx:() in
  Picos_structured.Run.all
    [
      Imp_batched_counter.par_incr_n counter 100;
      Imp_batched_counter.par_decr_n counter 100;
      Imp_batched_counter.par_get_n counter 100;
    ]

let () =
  let sched = try Sys.argv.(1) with _ -> "threaded" in
  Printf.printf "Running with %s scheduler\n\n%!" sched;
  match sched with
  | "threaded" -> Picos_threaded.run main
  | "fifos" -> Picos_fifos.run main
  | "randos" -> Picos_randos.run main
  | _ -> Printf.eprintf "Usage: %s <threaded | fifos | randos>" Sys.argv.(0)
