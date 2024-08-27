open Picos

module Batched = struct
  type t = int Atomic.t

  type cfg = unit
  let init ?cfg:_ () = Atomic.make 0

  type _ op = Incr : unit op | Decr : unit op | Get : int op
  type wrapped_op = Mk : 'a op * 'a Computation.t -> wrapped_op

  let run (t : t) (ops : wrapped_op array) =
    let len = Array.length ops in
    Logs.info (fun m -> m "Processing batch of %d operations" len);
    let start = Atomic.get t in
    let delta =
      Utils.parallel_for_reduce
        ~n_fibers:(Domain.recommended_domain_count () - 1)
        ~start:0 ~finish:(len - 1)
        ~body:(fun i ->
          match ops.(i) with
          | Mk (Incr, comp) ->
              Computation.return comp ();
              1
          | Mk (Decr, comp) ->
              Computation.return comp ();
              -1
          | Mk (Get, comp) ->
              Computation.return comp start;
              0)
        ( + ) 0
    in
    Atomic.set t (start + delta)
end

(* Set up implicit batching *)
include Obatcher.Make (Batched)

let incr t =
  Logs.debug (fun m -> m "Incr requested");
  exec t Incr;
  Logs.debug (fun m -> m "Incr completed")

let decr t =
  Logs.debug (fun m -> m "Decr requested");
  exec t Decr;
  Logs.debug (fun m -> m "Decr completed")

let get t =
  Logs.debug (fun m -> m "Got requested");
  let got = exec t Get in
  Logs.debug (fun m -> m "Got completed with %d" got);
  got
