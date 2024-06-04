open Domainslib
module BatchedCounter = Ds.Counter.Batched

let par_incr_n t pool n =
  Task.parallel_for ~start:1 ~finish:n
    ~body:(fun _ -> BatchedCounter.incr t)
    pool

let () =
  let n = 1_000_000 in
  let pool =
    Task.setup_pool ~num_domains:(Domain.recommended_domain_count ()) ()
  in
  Task.run pool (fun () ->
      let t = BatchedCounter.create pool in
      par_incr_n t pool n;
      BatchedCounter.incr t;
      BatchedCounter.decr t;
      let got = BatchedCounter.get t in
      Printf.printf "Got: %d, expected: %d" got n;
      assert (got = n));
  Task.teardown_pool pool
