(* Minimal Subset required *)
module type Sched_sig = sig
  type t
  type 'a promise

  val make_promise : unit -> 'a promise * ('a -> unit)
  val async : t -> (unit -> 'a) -> 'a promise
  val await : t -> 'a promise -> 'a
end

module type DS_sig = sig
  type sched
  (** [sched] represents the underlying scheduler, this is neccessary
      so we can enforce the constraints that the Make functor will use
      the same underlying scheduler *)

  type t
  (** [t] represents a vanilla data structure. *)

  type 'a op
  (** ['a op] represents a single operation on [t] with the return
      type ['a]. *)

  type wrapped_op =
    | Mk : 'a op * ('a -> unit) -> wrapped_op
        (** [wrapped_op] represents an operation on the datastructure and
        the continuation to run after its completion.  *)

  val init : unit -> t
  (** [init ()] returns a new instance of the data structure. *)

  val run : t -> sched -> wrapped_op array -> unit
  (** [run t sched ops num] when called with a data structure [t], and
      a thread sched [sched], executes all the operations in [ops],
      possibly using parallelism to improve the speed of the
      operation. *)
end

module Make (Sched : Sched_sig) (DS : DS_sig with type sched = Sched.t) = struct
  type 'a op = 'a DS.op

  type t = {
    sched : Sched.t;
    mutable ds : DS.t;
    running : bool Atomic.t;
    container : DS.wrapped_op Ts_container.t;
  }

  let init sched =
    {
      sched;
      ds = DS.init ();
      running = Atomic.make false;
      container = Ts_container.create ();
    }

  let rec try_launch t =
    if
      Ts_container.size t.container > 0
      && Atomic.compare_and_set t.running false true
    then (
      let batch = Ts_container.get t.container in
      DS.run t.ds t.sched batch;
      Atomic.set t.running false;
      try_launch t)

  let try_launch t =
    if
      Ts_container.size t.container > 0
      && Atomic.compare_and_set t.running false true
    then (
      let batch = Ts_container.get t.container in
      DS.run t.ds t.sched batch;
      Atomic.set t.running false;
      ignore @@ Sched.async t.sched (fun () -> try_launch t))

  let apply t op =
    let pr, set = Sched.make_promise () in
    let op_set = DS.Mk (op, set) in
    Ts_container.add t.container op_set;
    try_launch t;
    Sched.await t.sched pr
end
