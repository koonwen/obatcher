(* open Kcas_data *)

module type S = sig
  type t
  type 'a promise
  type 'a resolver

  val make_promise : unit -> ('a resolver * 'a promise)
  val async : t -> (unit -> 'a) -> 'a promise
  val await : t -> 'a promise -> 'a
end

(* module Domainslib : S = struct *)

(*   type t = { *)
(*     Domainslib.Task.pool *)
(*   } *)

(*   let make_promise () = *)
(*     Domainslib.Task. *)
(*   let async t f = async t f *)

(*   let *)
(* end *)
