module Scheduler = Domainslib.Task
module Batcher = Obatcher.Apply (Scheduler)

module Sequential = struct

  type t = { mutable counter: int }

  let init () = {counter=0}


  let incr t = t.counter <- t.counter + 1

  let decr t = t.counter <- t.counter - 1

  let get t = t.counter

end

module Counter : Batcher.DS = struct

  type t = Sequential.t

  type 'a op =
    | Incr : unit op
    | Decr : unit op
    | Get : int op

  type wrapped_op = Mk: 'a op * ('a -> unit) -> wrapped_op

  let init () : t = {counter=0}

  let run (t: t) (pool: Scheduler.pool) (ops: wrapped_op array) =
    let len = Array.length ops in
    let start = t.counter in
    let add_n = Scheduler.parallel_for_reduce pool ~start:0 ~finish:(len-1)
        ~body:(fun i -> match ops.(i) with
            | Mk (Incr, set) ->  set (); 1
            | Mk (Decr, set) ->  set (); -1
            | Mk (Get, set) -> set start; 0) (+) 0 in
    t.counter <- (start + add_n)
end

module BatchedCounter = Batcher.Make (Counter)

