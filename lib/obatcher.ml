open Domainslib

module type S = sig

  type t

  type 'a op

  type wrapped_op = Mk : 'a op * ('a -> unit) -> wrapped_op

  val init : unit -> t

  val run : t -> Task.pool -> wrapped_op array -> unit

end

module type S1 = sig

  type 'a t

  type ('a, 'b) op

  type 'a wrapped_op = Mk : ('a, 'b) op * ('b -> unit) -> 'a wrapped_op

  val init : unit -> 'a t

  val run : 'a t -> Task.pool -> 'a wrapped_op array -> unit

end


module Make (S : S) = struct
  type 'a op = 'a S.op
  type t = {
    pool : Task.pool;
    mutable ds : S.t;
    running : bool Atomic.t;
    container : S.wrapped_op Ts_container.t
  }

  let init pool =
    { pool;
      ds = S.init ();
      running = Atomic.make false;
      container = Ts_container.create () }


  let rec try_launch t =
    if Ts_container.size t.container > 0
    && Atomic.compare_and_set t.running false true
    then
      begin
        let batch = Ts_container.get t.container in
        S.run t.ds t.pool batch;
        Atomic.set t.running false;
        try_launch t
      end

  let try_launch t =
    if Ts_container.size t.container > 0
    && Atomic.compare_and_set t.running false true
    then
      begin
        let batch = Ts_container.get t.container in
        S.run t.ds t.pool batch;
        Atomic.set t.running false;
        ignore @@ Task.async t.pool (fun () -> try_launch t)
      end

  let apply t op =
    let pr, set = Task.promise () in
    let op_set = S.Mk (op, set) in
    Ts_container.add t.container op_set;
    try_launch t;
    Task.await t.pool pr

  let unsafe_get_internal_data t = t.ds
  [@@@alert unsafe "For developer use"]

  let unsafe_set_internal_data t ds =  t.ds <- ds
  [@@@alert unsafe "For developer use"]

end


module Make1 (S : S1) = struct
  type ('a,'b) op = ('a,'b) S.op
  type 'a t = {
    pool : Task.pool;
    mutable ds : 'a S.t;
    running : bool Atomic.t;
    container : 'a S.wrapped_op Ts_container.t
  }

  let init pool =
    { pool;
      ds = S.init ();
      running = Atomic.make false;
      container = Ts_container.create () }

  let rec try_launch t =
    if Ts_container.size t.container > 0
    && Atomic.compare_and_set t.running false true
    then
      begin
        let batch = Ts_container.get t.container in
        S.run t.ds t.pool batch;
        Atomic.set t.running false;
        try_launch t
      end

  let try_launch t =
    if Ts_container.size t.container > 0
    && Atomic.compare_and_set t.running false true
    then
      begin
        let batch = Ts_container.get t.container in
        S.run t.ds t.pool batch;
        Atomic.set t.running false;
        ignore @@ Task.async t.pool (fun () -> try_launch t)
      end

  let apply t op =
    let pr, set = Task.promise () in
    let op_set = S.Mk (op, set) in
    Ts_container.add t.container op_set;
    try_launch t;
    Task.await t.pool pr

  let unsafe_get_internal_data t = t.ds
  [@@@alert unsafe "For developer use"]

  let unsafe_set_internal_data t ds =  t.ds <- ds
  [@@@alert unsafe "For developer use"]

end
