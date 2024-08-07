module Counter = Imp_batched_counter

let n_fiber_incr t n () =
  let thunks = List.init n (fun _ () -> Counter.incr t) in
  Picos_structured.Run.all thunks

let n_fiber_decr t n () =
  let thunks = List.init n (fun _ () -> Counter.decr t) in
  Picos_structured.Run.all thunks

let n_fiber_get t n () =
  let thunks = List.init n (fun _ () -> Counter.get t |> Printf.printf "Got %d\n%!") in
  Picos_structured.Run.all thunks

let main () =
  let counter = Counter.init ~ctx:() in
  Picos_structured.Run.all
    [
      n_fiber_incr counter 1_000_000;
      n_fiber_decr counter 1_000_000;
      n_fiber_get counter 1_000_000;
    ]

let () =
  let sched = try Sys.argv.(1) with _ -> "threaded" in
  Printf.printf "Running with %s scheduler\n\n%!" sched;
  match sched with
  | "threaded" -> Picos_threaded.run main
  | "fifos" -> Picos_fifos.run main
  | "randos" ->
      let extra = Domain.recommended_domain_count () - 1 in
      let context = Picos_randos.context () in
      (* Spawn multiple domains with multiple threads each for this
         scheduler *)
      let spawn_ndoms_with_nthreads nd nt =
        for _ = 1 to nd do
          Domain.spawn (fun () ->
              for _ = 1 to nt do
                Thread.create Picos_randos.runner_on_this_thread context
                |> ignore
              done;
              Picos_randos.runner_on_this_thread context)
          |> ignore
        done
      in
      spawn_ndoms_with_nthreads extra extra;
      Printf.printf "Spawning %d domains with extra %d threads per domain\n%!"
        extra extra;
      Picos_randos.run ~context main
  | _ -> Printf.eprintf "Usage: %s <threaded | fifos | randos>\n" Sys.argv.(0)
