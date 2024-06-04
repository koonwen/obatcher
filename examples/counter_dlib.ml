(* Demonstrate how we can take a seq data structure and convert it to
   batched version *)
module Sequential = struct
  type t = { mutable cnt : int; ctx : Domainslib.Task.pool }

  let create ctx = { cnt = 0; ctx }
  let incr t = incr t
  let decr t = decr t
  let get t = !t
end
[@@warning "-32"]

module Batched = struct
  module S = struct
    type 'a resolver = 'a -> unit
    type sched_ctx = Domainslib.Task.pool
    type t = Sequential.t
    type 'a op = Incr : unit op | Decr : unit op | Get : int op
    type wrapped_op = Mk : 'a op * 'a resolver -> wrapped_op

    let init sched_ctx : t = Sequential.create sched_ctx

    let run (t : t) (ops : wrapped_op array) =
      let len = Array.length ops in
      let start = t.cnt in
      let add_n =
        Domainslib.Task.parallel_for_reduce t.ctx ~start:0 ~finish:(len - 1)
          ~body:(fun i ->
            match ops.(i) with
            | Mk (Incr, set) -> set (); 1
            | Mk (Decr, set) -> set (); -1
            | Mk (Get, set) ->  set start; 0)
          ( + ) 0
      in
      t.cnt <- start + add_n
  end
end

let () =
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains:2 () in
  (* Set up Backend *)
  let module Domainslib_PR = struct
    type 'a promise = 'a T.promise
    type 'a resolver = 'a -> unit

    let create = T.promise
    let resolve r v = r v
    let await pr = T.await pool pr
  end in
  let module BatchedCounter = struct
    module B = Obatcher.Make (Domainslib_PR) (Batched.S)

    let create pool = B.init pool
    let incr t = B.apply t Batched.S.Incr
    let decr t = B.apply t Batched.S.Decr
    let get t = B.apply t Batched.S.Get
  end in
  let n = 3000 in
  T.run pool (fun () ->
      let t = BatchedCounter.create pool in
      T.parallel_for pool ~start:1 ~finish:n ~body:(fun i ->
          if i mod 3 = 0 then BatchedCounter.decr t else BatchedCounter.incr t);
      let got = BatchedCounter.get t in
      Printf.printf "Got: %d, expected: %d" got (n/3) ;
      assert (got = n/3));
  T.teardown_pool pool
