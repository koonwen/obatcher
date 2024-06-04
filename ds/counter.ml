module Sequential = struct
  type t = int ref

  let create () = ref 0
  let incr t = incr t
  let decr t = decr t
  let get t = !t
end

module Batched = struct
  module S = struct
    type sched = Domainslib.Task.pool
    type t = Sequential.t
    type 'a op = Incr : unit op | Decr : unit op | Get : int op
    type wrapped_op = Mk : 'a op * ('a -> unit) -> wrapped_op

    let init () : t = Sequential.create ()

    let run (t : t) pool (ops : wrapped_op array) =
      let len = Array.length ops in
      let start = !t in
      let add_n =
        Domainslib.Task.parallel_for_reduce pool ~start:0 ~finish:(len - 1)
          ~body:(fun i ->
            match ops.(i) with
            | Mk (Incr, set) ->
                set ();
                1
            | Mk (Decr, set) ->
                set ();
                -1
            | Mk (Get, set) ->
                set start;
                0)
          ( + ) 0
      in
      t := start + add_n
  end

  module B = Obatcher.Make (struct
    include Domainslib.Task

    type t = pool

    let make_promise = promise
  end) (S)

  let create pool = B.init pool
  let incr t = B.apply t S.Incr
  let decr t = B.apply t S.Decr
  let get t = B.apply t S.Get
end
