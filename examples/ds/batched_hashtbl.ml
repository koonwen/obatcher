(* Flat-combining hashtable, with no fancy internal parallelism *)

open Picos

module Batched = struct
  type t = (int, string) Stdlib.Hashtbl.t

  type 'a op =
    | Add : int * string -> unit op
    | Replace : int * string -> unit op
    | Remove : int -> unit op
    | Find : int -> string op

  type wrapped_op = Mk : 'a op * 'a Computation.t -> wrapped_op
  type cfg = { random : bool option; initial_size : int }

  let init ?(cfg = { random = None; initial_size = 0 }) () =
    Stdlib.Hashtbl.create ?random:cfg.random cfg.initial_size

  let run t (batch : wrapped_op array) =
    Array.iter
      (function
        | Mk (Add (k, v), comp) ->
            Computation.return comp (Stdlib.Hashtbl.add t k v)
        | Mk (Replace (k, v), comp) ->
            Computation.return comp (Stdlib.Hashtbl.replace t k v)
        | Mk (Remove k, comp) ->
            Computation.return comp (Stdlib.Hashtbl.remove t k)
        | Mk (Find k, comp) -> Computation.return comp (Stdlib.Hashtbl.find t k))
      batch
end

(* Set up implicit batching *)
include Obatcher.Make (Batched)
