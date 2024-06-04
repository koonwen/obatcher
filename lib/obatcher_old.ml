(* module Task = Domainslib.Task *)
(* module Promise = Kcas_data.Promise *)

(* module type DS = sig *)

(*   type t *)

(*   type 'a op *)

(*   type wrapped_op = Mk : 'a op * ('a Promise.u) -> wrapped_op *)

(*   val init : unit -> t *)

(*   val run : t -> Task.pool -> wrapped_op array -> unit *)

(* end *)

(* module type DS_Poly = sig *)

(*   type 'a t *)

(*   type ('a, 'b) op *)

(*   type 'a wrapped_op = Mk : ('a, 'b) op * ('b Promise.u) -> 'a wrapped_op *)

(*   val init : unit -> 'a t *)

(*   val run : 'a t -> Task.pool -> 'a wrapped_op array -> unit *)

(* end *)


(* module Make (DS : DS) = struct *)
(*   type 'a op = 'a DS.op *)
(*   type t = { *)
(*     pool : Task.pool; *)
(*     mutable ds : DS.t; *)
(*     running : bool Atomic.t; *)
(*     container : DS.wrapped_op Ts_container.t *)
(*   } *)

(*   let init pool = *)
(*     { pool; *)
(*       ds = DS.init (); *)
(*       running = Atomic.make false; *)
(*       container = Ts_container.create () } *)


(*   let rec try_launch t = *)
(*     if Ts_container.size t.container > 0 *)
(*     && Atomic.compare_and_set t.running false true *)
(*     then *)
(*       begin *)
(*         let batch = Ts_container.get t.container in *)
(*         DS.run t.ds t.pool batch; *)
(*         Atomic.set t.running false; *)
(*         try_launch t *)
(*       end *)

(*   let try_launch t = *)
(*     if Ts_container.size t.container > 0 *)
(*     && Atomic.compare_and_set t.running false true *)
(*     then *)
(*       begin *)
(*         let batch = Ts_container.get t.container in *)
(*         DS.run t.ds t.pool batch; *)
(*         Atomic.set t.running false; *)
(*         ignore @@ Task.async t.pool (fun () -> try_launch t) *)
(*       end *)

(*   let apply t op = *)
(*     let pr, set = Promise.create () in *)
(*     let op_set = DS.Mk (op, set) in *)
(*     Ts_container.add t.container op_set; *)
(*     try_launch t; *)
(*     Promise.await pr *)

(*   let unsafe_get_internal_data t = t.ds *)
(*   [@@@alert unsafe "For developer use"] *)

(*   let unsafe_set_internal_data t ds =  t.ds <- ds *)
(*   [@@@alert unsafe "For developer use"] *)

(* end *)


(* module Make_Poly (DS : DS_Poly) = struct *)
(*   type ('a,'b) op = ('a,'b) DS.op *)
(*   type 'a t = { *)
(*     pool : Task.pool; *)
(*     mutable ds : 'a DS.t; *)
(*     running : bool Atomic.t; *)
(*     container : 'a DS.wrapped_op Ts_container.t *)
(*   } *)

(*   let init pool = *)
(*     { pool; *)
(*       ds = DS.init (); *)
(*       running = Atomic.make false; *)
(*       container = Ts_container.create () } *)

(*   let rec try_launch t = *)
(*     if Ts_container.size t.container > 0 *)
(*     && Atomic.compare_and_set t.running false true *)
(*     then *)
(*       begin *)
(*         let batch = Ts_container.get t.container in *)
(*         DS.run t.ds t.pool batch; *)
(*         Atomic.set t.running false; *)
(*         try_launch t *)
(*       end *)

(*   let try_launch t = *)
(*     if Ts_container.size t.container > 0 *)
(*     && Atomic.compare_and_set t.running false true *)
(*     then *)
(*       begin *)
(*         let batch = Ts_container.get t.container in *)
(*         DS.run t.ds t.pool batch; *)
(*         Atomic.set t.running false; *)
(*         ignore @@ Task.async t.pool (fun () -> try_launch t) *)
(*       end *)

(*   let apply t op = *)
(*     let pr, set = Promise.create () in *)
(*     let op_set = DS.Mk (op, set) in *)
(*     Ts_container.add t.container op_set; *)
(*     try_launch t; *)
(*     Promise.await pr *)

(*   let unsafe_get_internal_data t = t.ds *)
(*   [@@@alert unsafe "For developer use"] *)

(*   let unsafe_set_internal_data t ds =  t.ds <- ds *)
(*   [@@@alert unsafe "For developer use"] *)

(* end *)
