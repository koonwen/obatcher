open Picos

(* Higher level concurrency primatives implemented on top of Picos *)
let parallel_for_reduce ?(n_fibers = 1) ~start ~finish ~body reduce_fn init =
  let chunk_size =
    let sz = (finish - start + 1) / n_fibers in
    max sz 1
  in
  let rec work bundle s e =
    if e - s < chunk_size then
      let rec loop i acc =
        if i > e then acc else loop (i + 1) (reduce_fn acc (body i))
      in
      loop (s + 1) (body s)
    else
      let d = s + ((e - s) / 2) in
      let p =
        Picos_structured.Bundle.fork_as_promise bundle (fun _ ->
            work bundle s d)
      in
      let right = work bundle (d + 1) e in
      let left = Picos_structured.Promise.await p in
      reduce_fn left right
  in
  if finish < start then init
  else
    reduce_fn init
      (Picos_structured.Bundle.join_after (fun bundle ->
           work bundle start finish))

module Batched = struct
  type t = int Atomic.t

  let init ~ctx:_ = Atomic.make 0

  type _ op = Incr : unit op | Decr : unit op | Get : int op
  type wrapped_op = Mk : 'a op * 'a Computation.t -> wrapped_op

  let run (t : t) (ops : wrapped_op array) =
    let len = Array.length ops in
    Logs.info (fun m -> m "Processing batch of %d operations" len);
    let start = Atomic.get t in
    let delta =
      parallel_for_reduce
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
  apply t Incr;
  Logs.debug (fun m -> m "Incr completed")

let decr t =
  Logs.debug (fun m -> m "Decr requested");
  apply t Decr;
  Logs.debug (fun m -> m "Decr completed")

let get t =
  Logs.debug (fun m -> m "Got requested");
  let got = apply t Get in
  Logs.debug (fun m -> m "Got completed with %d" got);
  got
