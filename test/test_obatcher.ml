open QCheck2

(* Picos available schedulers *)
type scheduler =
  | Threaded (* Forks every fiber into it's own unix thread *)
  | Fifos (* Queuing discipline *)
  | Randos (* Randomly selects work form the queue *)

let show_scheduler = function
  | Threaded -> "threaded"
  | Fifos -> "fifos"
  | Randos -> "randos"

let run_with sched f =
  match sched with
  | Threaded -> Picos_threaded.run f
  | Fifos -> Picos_fifos.run f
  | Randos -> Picos_randos.run f

type 'a prop = 'a -> bool

module Mock_Service_Batched = struct
  type t = unit
  type cfg
  type 'a op = Nop : int -> unit op
  type wrapped_op = Mk : 'a op * 'a Picos.Computation.t -> wrapped_op

  let init ?cfg:_ () = ()

  let run _t ops =
    Array.iter
      (function Mk (Nop _, comp) -> Picos.Computation.return comp ())
      ops
end

module Mock_Service = Obatcher.Make (Mock_Service_Batched)

(* let gen_tests_forall_sched_with (cells : ('a prop -> _ Test.cell) list) = *)
(*   List.map (fun cell -> *)
(*       List.map (fun sched -> *)
(*           let gen_name = *)
(*             Printf.sprintf "%s (with %s scheduler)" (Test.get_name cell) *)
(*               (show_scheduler sched) *)
(*           in *)
(*           Test.set_name cell gen_name; *)
(*           let law = Test.get_law cell in *)
(*         ) [ Threaded; Fifos; Randos ]) cells *)

let test_one_domain_one_fiber = failwith ""
let test_n_domains_one_fiber = failwith ""
let test_one_domain_n_fibers = failwith ""
let test_n_domains_and_n_fibers = failwith ""

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [
        test_one_domain_one_fiber;
        test_n_domains_one_fiber;
        test_one_domain_n_fibers;
        test_n_domains_and_n_fibers;
      ]
  in
  Alcotest.run "Obatcher"
    [
      ("test_one_domain_one_fiber", suite);
      ("test_one_domain_n_fiber", suite);
      ("test_n_domain_one_fiber", suite);
      ("test_n_domain_n_fiber", suite);
    ]
