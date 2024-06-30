type 'a t = { q : 'a Picos_mpscq.t; size : int Atomic.t; batch_limit : int }

(* Change batch limit to (N)processes *)
let create ?(batch_limit = max_int) () =
  { q = Picos_mpscq.create (); size = Atomic.make 0; batch_limit }

let add t elt =
  Atomic.incr t.size;
  Picos_mpscq.push t.q elt

let get t =
  let batch_size = Atomic.exchange t.size 0 in
  let limit = min batch_size t.batch_limit in
  let topup = batch_size - limit in
  let _ = Atomic.fetch_and_add t.size topup in
  Array.init limit (fun _ -> Picos_mpscq.pop_exn t.q)

let size t = Atomic.get t.size
