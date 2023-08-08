open Domainslib

module ChanBased = struct
  type 'a t = {
    chan : 'a Chan.t;
    size : int Atomic.t;
    batch_limit : int
  }

  (* Change batch limit to (N)processes *)
  let create ?(batch_limit=max_int) () =
    {
      chan = Chan.make_unbounded ();
      size = Atomic.make 0;
      batch_limit;
    }
  let add t elt =
    ignore @@ Atomic.fetch_and_add t.size 1;
    Chan.send t.chan elt
  let get t =
    let batch_size = Atomic.exchange t.size 0 in
    let limit = min batch_size t.batch_limit in
    let topup = batch_size - limit in
    let _ = Atomic.fetch_and_add t.size topup in
    Array.init limit (fun _ -> Chan.recv t.chan)
  let size t = Atomic.get t.size
end

include ChanBased
