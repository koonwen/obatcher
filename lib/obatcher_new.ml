module Promise = Kcas_data.Promise

module type Sched = sig
  type t

  val async : (unit -> 'a) -> 'a
end

module Sched_seq : Sched = struct
  type t = unit

  let async f = f ()
end

module DS (Sched : Sched) = struct
  type t
  type 'a op
  type wrapped_op = Mk : 'a op * 'a Promise.u -> wrapped_op

  let init : unit -> t = failwith ""
  let run : t -> Sched.t -> wrapped_op array -> unit = failwith ""
end
