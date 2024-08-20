module type Service = sig
  type t
  type cfg
  type 'a op
  type wrapped_op = Mk : 'a op * 'a Picos.Computation.t -> wrapped_op

  val init : ?cfg:cfg -> unit -> t
  val run : t -> wrapped_op array -> unit
end

module Make (S : Service) = struct

  type t = {
    mutable ds : S.t;
    running : bool Atomic.t;
    container : S.wrapped_op Ts_container.t;
  }

  let init ?cfg () =
    {
      ds = S.init ?cfg ();
      running = Atomic.make false;
      container = Ts_container.create ();
    }

  let exec t op =
    let open Picos in
    let comp = Computation.create () in
    let op_set = S.Mk (op, comp) in
    Ts_container.add t.container op_set;
    (* Try launching batch *)
    while Computation.peek comp = None do
      if
        Ts_container.size t.container > 0
        && Atomic.compare_and_set t.running false true
      then (
        (* Batching Fiber *)
        let batch = Ts_container.get t.container in
        S.run t.ds batch;
        Atomic.set t.running false)
      else
        (* A batch is being processed, yield and try again later *)
        Fiber.yield ()
    done;
    Computation.await comp
end
