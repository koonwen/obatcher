module type Promise_sig = sig
  type 'a promise
  type 'a resolver

  val create : unit -> 'a promise * 'a resolver
  val resolve : 'a resolver -> 'a -> unit
  val await : 'a promise -> 'a
end

module type DS_sig = sig
  type sched_ctx

  type 'a resolver
  (** [resolver] represents the underlying transaction, this is
      neccessary so we can enforce the constraints that the Make
      functor will use the same underlying transaction model *)

  type t
  (** [t] represents a vanilla data structure. *)

  type 'a op
  (** ['a op] represents a single operation on [t] with the return
      type ['a]. *)

  type wrapped_op =
    | Mk : 'a op * 'a resolver -> wrapped_op
        (** [wrapped_op] represents an operation on the datastructure and
        the continuation to run after its completion.  *)

  val init : sched_ctx -> t
  (** [init ()] returns a new instance of the data structure. *)

  val run : t -> wrapped_op array -> unit
  (** [run t sched ops num] when called with a data structure [t], and
      a thread sched [sched], executes all the operations in [ops],
      possibly using parallelism to improve the speed of the
      operation. *)
end

module Make
    (PR : Promise_sig)
    (DS : DS_sig with type 'a resolver = 'a PR.resolver) =
struct
  type 'a op = 'a DS.op

  type t = {
    mutable ds : DS.t;
    running : bool Atomic.t;
    container : DS.wrapped_op Ts_container.t;
  }

  let init ctx =
    {
      ds = DS.init ctx;
      running = Atomic.make false;
      container = Ts_container.create ();
    }

  let rec try_launch t =
    if
      Ts_container.size t.container > 0
      && Atomic.compare_and_set t.running false true
    then (
      (* Launching *)
      let batch = Ts_container.get t.container in
      DS.run t.ds batch;
      Atomic.set t.running false;
      try_launch t)

  let apply t op =
    let pr, set = PR.create () in
    let op_set = DS.Mk (op, set) in
    Ts_container.add t.container op_set;
    try_launch t;
    PR.await pr
end
