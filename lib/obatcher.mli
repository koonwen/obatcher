module type Service = sig
  type t
  (** [t] represents the underlying service that processes and handles
      batches of operations as input. Only a single batch is active at
      any time *)

  type cfg
  (** [cfg] represents config variables that you want to expose
      to users. *)

  type 'a op
  (** ['a op] represents an single operation on [t] with the return
      type ['a]. *)

  type wrapped_op =
    | Mk : 'a op * 'a Picos.Computation.t -> wrapped_op
    (** [wrapped_op] binds the operation on the service with it's
        corresponding suspended continuation to run after its
        completion.  *)

  (* Add max_batsz parameter *)
  (* val max_batch_sz : int option *)
  (* (\** Configure the maximum batch size that the service will handle at *)
  (*     any time *\) *)

  val init : ?cfg:cfg -> unit -> t
  (** [init ()] returns a new instance of the service. *)

  val run : t -> wrapped_op array -> unit
  (** [run t ops] when called on service [t], processes all the
      operations in [ops], possibly using parallelism to complete the
      batch. *)
end

module Make : functor (S : Service) -> sig
  type t
  (** [t] represents the service now abstracted with batching made
      implicit & automatic *)

  (* May be useful to add a timeout parameter if requests don't need to be handled immediately *)
  val init : ?cfg:S.cfg -> unit -> t
  (** [init ?cfg ()] initializes service with implicit batching. [cfg]
      is passed on the service initialization *)

  val exec : t -> 'a S.op -> 'a
  (** [exec t op] is the API call for a singular operation on the
      service with operations being automatically batched before
      passed to the service *)

  val get_internal : t -> S.t
  [@@alert unsafe "Should not be used typically by application"]
  (** [get_internal t] extracts underlying internal service *)
end
