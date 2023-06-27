module type Scheduler = sig
  type 'a task = unit -> 'a
  (** Type of task *)

  type !'a promise
  (** Type of promises *)

  type pool
  (** Type of task pool *)

  val async : pool -> 'a task -> 'a promise
  (** [async p t] runs the task [t] asynchronously in the pool [p]. The function returns a promise [r] in which the result of the task [t] will be stored. *)

  val await : pool -> 'a promise -> 'a
  (** [await p r] waits for the promise [r] to be resolved. During the resolution, other tasks in the pool [p] might be run using the calling domain and/or the domains in the pool [p]. If the task associated with the promise have completed successfully, then the result of the task will be returned. If the task have raised an exception, then [await] raises the same exception. Must be called with a call to {!run} in the dynamic scope to handle the internal algebraic effects for task synchronization. *)

  val promise : unit -> 'a promise * ('a -> unit)
  (** [promise ()] returns a new promise and a function to resolve it to a value. The function can only be called once. *)
end

module Apply (Task : Scheduler) : sig
  module type DS = sig
    type t
    (** [t] represents a vanilla data structure. *)

    type 'a op
    (** ['a op] represents a single operation on [t] with the return type ['a]. *)

    type wrapped_op =
      | Mk : 'a op * ('a -> unit) -> wrapped_op
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
      | Mk : ('a, 'b) op * ('b -> unit) -> 'a wrapped_op
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
end
