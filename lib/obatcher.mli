module Task = Domainslib.Task
module Promise = Kcas_data.Promise

module type DS = sig
  type t
  (** [t] represents a vanilla data structure. *)

  type 'a op
  (** ['a op] represents a single operation on [t] with the return type ['a]. *)

  type wrapped_op =
    | Mk : 'a op * ('a Promise.u) -> wrapped_op
    (** [wrapped_op] represents an operation on the datastructure and
        the continuation to run after its completion.  *)

  val init : unit -> t
  (** [init ()] returns a new instance of the data structure. *)

  val run : t -> Task.pool -> wrapped_op array -> unit
  (** [run t pool ops num] when called with a data structure [t], and
      a thread pool [pool], executes all the operations in [ops],
      possibly using parallelism to improve the speed of the
      operation. *)
end

module type DS_Poly = sig
  type 'a t
  (** ['a t] represents a data structure parameterised over ['a]. *)

  type ('a, 'b) op
  (** [('a, 'b) op] represents a single operation on ['a t] with the return type ['b]. *)

  type 'a wrapped_op =
    | Mk : ('a, 'b) op * ('b Promise.u) -> 'a wrapped_op
    (** ['a wrapped_op] represents an operation on the datastructure ['a
        t] and the continuation to run after its completion.  *)

  val init : unit -> 'a t
  (** [init ()] returns a new instance of the data structure. *)

  val run : 'a t -> Task.pool -> 'a wrapped_op array -> unit
  (** [run t pool ops num] when called with a data structure ['a t], and
      a thread pool [pool], executes all the operations in [ops],
      possibly using parallelism to improve the speed of the
      operation. *)
end

module Make : functor (DS : DS) -> sig
  type t
  (** [t] represents the type of a concurrent data structure *)

  type 'a op = 'a DS.op
  (** ['a op] represents an operation  *)

  val init : Task.pool -> t
  (** [init pool] creates a new batched data structure, where [pool]
      will be used for parallelism. *)

  val apply : t -> 'a op -> 'a
  (** [apply t op] applies the operation [op] to [t]. *)

  val unsafe_get_internal_data : t -> DS.t

  [@@@alert unsafe "For developer use"]

  val unsafe_set_internal_data : t -> DS.t -> unit

  [@@@alert unsafe "For developer use"]
end

module Make_Poly : functor (DS : DS_Poly) -> sig
  type 'a t
  (** ['a t] represents the type of a concurrent data structure *)

  type ('a, 'b) op = ('a, 'b) DS.op
  (** [('a,'b) op] represents an operation.  *)

  val init : Task.pool -> 'a t
  (** [init pool] creates a new batched data structure, where [pool]
      will be used for parallelism. *)

  val apply : 'a t -> ('a, 'b) op -> 'b
  (** [apply t op] applies the operation [op] to [t]. *)

  val unsafe_get_internal_data : 'a t -> 'a DS.t

  [@@@alert unsafe "For developer use"]

  val unsafe_set_internal_data : 'a t -> 'a DS.t -> unit

  [@@@alert unsafe "For developer use"]
end
