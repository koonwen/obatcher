open Obatcher
module Skiplist = Ds.Skiplist.Make (Int)
module BatchedSkiplist = Skiplist.Batched

let par_insert_n t pool n =
  Task.parallel_for ~start:1 ~finish:n
    ~body:(fun i -> BatchedSkiplist.insert t i)
    pool

let () =
  let n = 1_000_000 in
  let pool =
    Task.setup_pool ~num_domains:(Domain.recommended_domain_count ()) ()
  in
  Task.run pool (fun () ->
      let t = BatchedSkiplist.create pool in
      par_insert_n t pool n;
      let got = BatchedSkiplist.size t in
      Printf.printf "Got: %d, expected: %d" got n;
      assert (got = n));
  Task.teardown_pool pool
