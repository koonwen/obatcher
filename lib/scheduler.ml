open Kcas_data

module type S = sig
  type t
  type 'a promise
  type 'a resolver

  val make_promise : unit -> ('a resolver * 'a promise)
  val async : t -> (unit -> 'a) -> 'a promise
  val await : t -> 'a promise -> 'a
end

module Seq = struct
  type t = unit
  type 'a promise = 'a resolver ref
  type 'a resolver = 'a

  let async t f = Promise.
end

module Domainslib = struct
  open Domainslib.Task

  type t = pool

  let async t f = async t f
end
