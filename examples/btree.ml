open Obatcher
module Btree = Ds.Btree.Make (Int)
module BatchedBtree = Btree.Batched

let par_incr_n t pool n =
  Task.parallel_for ~start:1 ~finish:n
    ~body:(fun i -> BatchedBtree.insert t i i)
    pool

let () =
  let n = 1_000_000 in
  let pool =
    Task.setup_pool ~num_domains:(Domain.recommended_domain_count ()) ()
  in
  Task.run pool (fun () ->
      let t = BatchedBtree.create pool in
      par_incr_n t pool n;
      let got = BatchedBtree.size t in
      Printf.printf "Got: %d, expected: %d" got n;
      assert (got = n));
  Task.teardown_pool pool
