module Task = Domainslib.Task
module Batcher = Obatcher.Apply(Task)
module Counter = struct

  type t = { mutable counter: int }

  type 'a op =
    | Incr : unit op
    | Decr : unit op
    | Get : int op

  type wrapped_op = Mk: 'a op * ('a -> unit) -> wrapped_op

  let init () : t = {counter=0}

  let run (t: t) (pool: Task.pool) (ops: wrapped_op array) =
    let len = Array.length ops in
    let start = t.counter in
    let add_n = Task.parallel_for_reduce pool ~start:0 ~finish:(len-1)
        ~body:(fun i -> match ops.(i) with
            | Mk (Incr, set) ->  set (); 1
            | Mk (Decr, set) ->  set (); -1
            | Mk (Get, set) -> set start; 0) (+) 0 in
    t.counter <- (start + add_n)
end

let () =
  let module BatchedCounter = Batcher.Make (Counter) in
  let pool = Task.setup_pool ~num_domains:(Domain.recommended_domain_count ()) () in
  let t = BatchedCounter.init pool in
  BatchedCounter.apply t Incr;
  BatchedCounter.apply t Decr;
  BatchedCounter.apply t Get |> print_int
