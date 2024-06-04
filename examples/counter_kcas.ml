(* Demonstrate how we can take a seq data structure and convert it to
   batched version *)
module Sequential = struct
  type t = int ref

  let create _ctx = ref 0
  let incr t = incr t
  let decr t = decr t
  let get t = !t
end
[@@warning "-32"]

module Batched = struct
  module S = struct
    type 'a resolver = 'a Kcas_data.Promise.u
    type sched_ctx = unit
    type t = Sequential.t
    type 'a op = Incr : unit op | Decr : unit op | Get : int op
    type wrapped_op = Mk : 'a op * 'a resolver -> wrapped_op

    let init sched_ctx : t = Sequential.create sched_ctx

    let run (t : t) (ops : wrapped_op array) =
      let len = Array.length ops in
      let before = !t in
      let add_n ~start ~finish =
        let total = ref 0 in
        let i = ref start in
        while !i < finish do
          let delta =

            match ops.(!i) with
            | Mk (Incr, set) -> Kcas_data.Promise.resolve set (); 1
            | Mk (Decr, set) -> Kcas_data.Promise.resolve set (); -1
            | Mk (Get, set) ->  Kcas_data.Promise.resolve set before; 0 in
          total := !total + delta;
          incr i
        done;
        !total
      in
      let half = len / 2 in
      let d1 = Domain.spawn (fun () -> add_n ~start:0 ~finish:half) in
      let d2 = Domain.spawn (fun () -> add_n ~start:half ~finish:(len)) in
      let front = Domain.join d1 in
      let back = Domain.join d2 in
      t := before + front + back
  end
end

let () =
  (* Set up Backend *)
  let module Kcas_PR = struct
    type 'a promise = 'a Kcas_data.Promise.t
    type 'a resolver = 'a Kcas_data.Promise.u

    let create = Kcas_data.Promise.create
    let resolve = Kcas_data.Promise.resolve
    let await = Kcas_data.Promise.await
  end in
  let module BatchedCounter = struct
    module B = Obatcher.Make (Kcas_PR) (Batched.S)

    let create () = B.init ()
    let incr t = B.apply t Batched.S.Incr
    let decr t = B.apply t Batched.S.Decr
    let get t = B.apply t Batched.S.Get
  end in
  let n = 1_000 in
  let t = BatchedCounter.create () in
  let d1 = Domain.spawn (fun () -> for _ = 1 to n do BatchedCounter.incr t done) in
  let d2 = Domain.spawn (fun () -> for _ = 1 to n do BatchedCounter.incr t done) in
  let d3 = Domain.spawn (fun () -> for _ = 1 to n do BatchedCounter.decr t done) in
  Domain.join d1 ; Domain.join d2; Domain.join d3;
  let got = BatchedCounter.get t in
  Printf.printf "Got: %d, expected: %d" got n;
  assert (got = n)
