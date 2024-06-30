open Picos

let incr_n t n () =
  let thunks = List.init n (fun _ () -> Imp_batched_counter.incr t) in
  Picos_structured.Run.all thunks

let decr_n t n () =
  let thunks = List.init n (fun _ () -> Imp_batched_counter.decr t) in
  Picos_structured.Run.all thunks

let get_n t n () =
  let thunks =
    List.init n (fun _ () ->
        Imp_batched_counter.get t |> Printf.printf "Got %d\n%!")
  in
  Picos_structured.Run.all thunks

let () =
  Picos_threaded.run @@ fun () ->
  let counter = Imp_batched_counter.init ~ctx:() in
  Picos_structured.Run.all
    [
      (* Spawn 3 parallel threads to handle each thunk *)
      incr_n counter 100;
      decr_n counter 100;
      get_n counter 100;
    ]
