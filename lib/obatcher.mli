module type DS_sig = sig
  type t
  (** [t] represents a vanilla data structure. *)

  type 'a op
  (** ['a op] represents a single operation on [t] with the return
      type ['a]. *)

  (* Would it be possible to use polymorphic variants? *)

  type wrapped_op =
    | Mk : 'a op * 'a Picos.Computation.t -> wrapped_op
        (** [wrapped_op] represents an operation on the datastructure and
        the continuation to run after its completion.  *)

  val init : ctx:'a -> t
  (** [init ()] returns a new instance of the data structure. *)

  val run : t -> wrapped_op array -> unit
  (** [run t ops num] when called with a data structure [t], executes
      all the operations in [ops], possibly using parallelism to
      improve the speed of the operation. *)
end

module Make : functor (DS : DS_sig) -> sig
  type 'a op = 'a DS.op
  type t

  val init : ctx:'a -> t
  val apply : t -> 'a op -> 'a
end
