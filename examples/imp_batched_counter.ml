open Picos

module BatchedCounter = struct
  type t = int Atomic.t

  let init ~ctx:_ = Atomic.make 0

  type _ op = Incr : unit op | Decr : unit op | Get : int op
  type wrapped_op = Mk : 'a op * 'a Computation.t -> wrapped_op

  (* Internal parallelism in counter *)
  let run (t : t) (ops : wrapped_op array) =
    let start = Atomic.get t in
    let thunks =
      Array.mapi
        (fun i op ->
          match op with
          | Mk (Incr, comp) ->
              fun () ->
                Printf.printf "running Incr slot %d\n%!" i;
                Unix.sleepf 1.0;
                Atomic.incr t;
                Computation.return comp ()
          | Mk (Decr, comp) ->
              fun () ->
                Printf.printf "running Decr slot %d\n%!" i;
                Unix.sleepf 1.0;
                Atomic.decr t;
                Computation.return comp ()
          | Mk (Get, comp) ->
              fun () ->
                Printf.printf "running Get slot %d\n%!" i;
                Unix.sleepf 1.0;
                Computation.return comp start)
        ops
    in
    Picos_structured.Run.all (thunks |> Array.to_list)
end

(* Set up implicit batching *)
include Obatcher.Make (BatchedCounter)

let incr t = apply t Incr
let decr t = apply t Decr
let get t = apply t Get

let par_incr_n t n () =
  let thunks = List.init n (fun _ () -> incr t) in
  Picos_structured.Run.all thunks

let par_decr_n t n () =
  let thunks = List.init n (fun _ () -> decr t) in
  Picos_structured.Run.all thunks

let par_get_n t n () =
  let thunks = List.init n (fun _ () -> get t |> Printf.printf "Got %d\n%!") in
  Picos_structured.Run.all thunks
