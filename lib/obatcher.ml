module type DS_sig = sig
  type t
  type 'a op
  type wrapped_op = Mk : 'a op * 'a Picos.Computation.t -> wrapped_op

  val init : ctx:'a -> t
  val run : t -> wrapped_op array -> unit
end

module Make (DS : DS_sig) = struct
  type 'a op = 'a DS.op

  type t = {
    mutable ds : DS.t;
    running : bool Atomic.t;
    container : DS.wrapped_op Ts_container.t;
  }

  let init ~ctx =
    {
      ds = DS.init ~ctx;
      running = Atomic.make false;
      container = Ts_container.create ();
    }

  let apply t op =
    let open Picos in
    let comp = Computation.create () in
    let op_set = DS.Mk (op, comp) in
    Ts_container.add t.container op_set;
    (* Try launching batch *)
    while Computation.peek comp = None do
      if
        Ts_container.size t.container > 0
        && Atomic.compare_and_set t.running false true
      then (
        (* Batching Fiber *)
        let batch = Ts_container.get t.container in
        DS.run t.ds batch;
        Atomic.set t.running false)
      else
        (* A batch is being processed, yield and try again later *)
        Fiber.yield ()
    done;
    Computation.await comp
end
