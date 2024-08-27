open QCheck2

(** Add this module just to get a fiber uid easily *)
module Picos_fiber : sig
  type id = private int

  val get_id : unit -> id
  (** [get_id ()] returns a unique integer id for the current fiber. *)

  val reset_id_counter : unit -> unit
  (** [reset_id_counter] resets id generator. Only call this in a
      sequential part of a program *)
end = struct
  type id = int

  let next = Atomic.make 0

  let fiber_id_key =
    Picos.Fiber.FLS.new_key @@ Computed (fun () -> Atomic.fetch_and_add next 1)

  let get_id () = Picos.Fiber.FLS.get (Picos.Fiber.current ()) fiber_id_key
  let reset_id_counter () = Atomic.set next 0
end

module Mock_Service_Internal = struct
  type 'a op = Req : int * Domain.id * Picos_fiber.id -> unit op
  type t = unit op array Queue.t
  (* Collect all batches
     seen for test
     inspection *)

  type cfg
  type wrapped_op = Mk : 'a op * 'a Picos.Computation.t -> wrapped_op

  let init ?cfg:_ () = Queue.create ()

  let pp_wrop ppf = function
    | Mk (Req (rid, did, fid), _) ->
        Fmt.pf ppf "Req (%i, %i, %i)" rid (fid :> int) (did :> int)

  let pp_ops = Fmt.(array ~sep:semi pp_wrop)

  let run t ops =
    Logs.info (fun m -> m "Domain %d is the batcher%!" (Domain.self () :> int));
    Logs.debug (fun m -> m "Batch of [%a]%!" pp_ops ops);
    (* Unix.sleepf 0.1; *)
    let reqs =
      Array.map
        (function
          | Mk ((Req _ as r), comp) ->
              Picos.Computation.return comp ();
              r)
        ops
    in
    Queue.add reqs t;
    Logs.debug (fun m -> m "Batch processing complete%!")

  let get_batches (t : t) = Queue.to_seq t |> Array.of_seq

  let batch_stats (t : t) =
    let batches = get_batches t in
    let batch_sizes = Array.map (fun a -> Array.length a) batches in
    let tbl = Hashtbl.create (Queue.length t) in
    Array.iter
      (fun key ->
        match Hashtbl.find_opt tbl key with
        | None -> Hashtbl.add tbl key 0
        | Some v -> Hashtbl.replace tbl key (v + 1))
      batch_sizes;
    let stats = Hashtbl.to_seq tbl |> Array.of_seq in
    Array.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2) stats;
    stats

  let _pp_stats = Fmt.(array ~sep:semi (pair int int))

  let print_stats_as_hist stats =
    let print_bar key counts = Fmt.pr "%d |%s\n" key (String.make counts '@') in
    let hi = stats.(Array.length stats - 1) |> fst in
    let idx = ref 0 in
    for i = 1 to hi do
      let k, v = stats.(!idx) in
      if k = i then (
        print_bar k v;
        incr idx)
      else print_bar i 0
    done
end

module Mock_Service = Obatcher.Make (Mock_Service_Internal)

(* Picos available schedulers *)
type scheduler =
  | Threaded (* Forks every fiber into it's own unix thread *)
  | Fifos (* Queuing discipline *)
  | Randos (* Randomly selects work form the queue *)

let show_scheduler = function
  | Threaded -> "threaded"
  | Fifos -> "fifos"
  | Randos -> "randos"

(* Iteratively submit n req operations *)
let submit_n_reqs t n =
  let did = Domain.self () in
  let did' = (did :> int) in
  let fid = Picos_fiber.get_id () in
  let fid' = (fid :> int) in
  for rid = 1 to n do
    Logs.app (fun m ->
        m "Submitting Req %d, Domain (%d), Fiber (%d)%!" rid did' fid');
    Mock_Service.exec t (Mock_Service_Internal.Req (rid, did, fid));
    Logs.app (fun m ->
        m "Completed Req %d, Domain (%d), Fiber (%d)!" rid did' fid')
  done

(** Split [n_req] to be run in [n_fibers] *)
let submit_f t ~n_fibers ~n_req =
  assert (n_req >= n_fibers);
  let block = n_req / n_fibers in
  let independent_ops = ref [] in
  let left = ref n_req in
  for i = 1 to n_fibers do
    let b = if i = n_fibers then !left else block in
    left := !left - b;
    assert (!left >= 0);
    independent_ops := (fun () -> submit_n_reqs t b) :: !independent_ops
  done;
  Picos_structured.Run.all !independent_ops

(** Setup function [f] to run within [n_domains] each running an
    instance of the specified scheduler *)
let run_with ~sched ~n_domains ~f =
  let n_domains_to_spawn = n_domains - 1 in
  let spawn_multiple sched_run =
    sched_run @@ fun () ->
    let extra_domains =
      List.init n_domains_to_spawn (fun _ ->
          Logs.app (fun m -> m "Spawning new domain");
          Domain.spawn (fun () -> sched_run f))
    in
    f ();
    List.iter Domain.join extra_domains
  in
  match sched with
  | Fifos -> spawn_multiple Picos_fifos.run
  | Threaded -> spawn_multiple Picos_threaded.run
  | Randos ->
      let context = Picos_randos.context () in
      let domains =
        List.init n_domains_to_spawn (fun _ ->
            Domain.spawn (fun () -> Picos_randos.runner_on_this_thread context))
      in
      let f_lst = List.init n_domains (fun _ -> f) in
      Picos_randos.run ~context (fun () -> Picos_structured.Run.all f_lst);
      List.iter Domain.join domains

(** Runs [n_req] submissions per domain with a [sched] instance
    running in each domain. [n_fibers] per domain is used to submit the requests *)
let run_submissions mock_service ~sched ~n_domains ~n_fibers ~n_req =
  assume (n_req >= n_fibers);
  Logs.info (fun m ->
      m
        "Running %s scheduler on %i domain(s) each with %i fiber(s) to handle \
         %d req per domain"
        (show_scheduler sched) n_domains n_fibers n_req);
  run_with ~sched ~n_domains ~f:(fun () ->
      submit_f mock_service ~n_fibers ~n_req)

(* Input generators *)
let small_nat_nonzero =
  let open Gen in
  int_bound 20 >|= fun i -> i + 1

(* Test config *)
type 'a testcase =
  Mock_Service.t ->
  sched:scheduler ->
  n_domains:int ->
  n_fibers:int ->
  'a ->
  bool

(* Test functions *)
let test_fn_req_sub_comp t ~sched ~n_domains ~n_fibers n_req =
  run_submissions t ~sched ~n_domains ~n_fibers ~n_req;
  let internal = Mock_Service.get_internal t in
  let batches_seen = Mock_Service_Internal.get_batches internal in
  let n_req_seen =
    Array.fold_left (fun acc batch -> Array.length batch + acc) 0 batches_seen
  in
  let total_req_submitted = n_req * n_domains in
  Logs.info (fun m -> m "Got %d n_req_seen\n" n_req_seen);
  n_req_seen = total_req_submitted

let test_fn_lin t ~sched ~n_domains ~n_fibers n_req =
  run_submissions t ~sched ~n_domains ~n_fibers ~n_req;
  let internal = Mock_Service.get_internal t in
  let batches_seen = Mock_Service_Internal.get_batches internal in
  let seen = Array.(concat (to_list batches_seen)) in
  let tbl = Hashtbl.create (n_fibers * n_domains) in
  Array.iter
    (function
      | Mock_Service_Internal.Req (rid, fid, did) -> (
          match Hashtbl.find_opt tbl (fid, did) with
          | Some l -> Hashtbl.replace tbl (fid, did) (rid :: l)
          | None -> Hashtbl.replace tbl (fid, did) [ rid ]))
    seen;
  let split_req_seen_by_fid_did =
    Hashtbl.to_seq tbl |> List.of_seq
    |> List.map (fun (k, v) -> (k, List.rev v))
  in
  Fmt.pr "Split req:\n";
  List.for_all
    (fun (((did, fid), req_l) : (Domain.id * Picos_fiber.id) * int list) ->
      Fmt.pr "(%i, %i): [%a]\n"
        (did :> int)
        (fid :> int)
        Fmt.(list ~sep:semi int)
        req_l;
      List.sort Int.compare req_l = req_l)
    split_req_seen_by_fid_did

(* Properties to test *)
let test_fn_lst =
  [
    ("Req submitted = Req completed", small_nat_nonzero, test_fn_req_sub_comp);
    ("Batches are linearizable", small_nat_nonzero, test_fn_lin);
  ]

let test_fn_all_for sched =
  let testcase_wrapper testcase input =
    (* Reset id generator so fiber ids appear starting from 0 for each new run *)
    Picos_fiber.reset_id_counter ();
    let mock_service = Mock_Service.init () in
    let result = testcase mock_service input in
    let internal = Mock_Service.get_internal mock_service in
    let batch_stats = Mock_Service_Internal.batch_stats internal in
    Fmt.pr "Histogram:\n";
    Mock_Service_Internal.print_stats_as_hist batch_stats;
    Fmt.pr "\n\n%!";
    result
  in
  (* Generate each test for 4 different configurations
     1. one domain one fiber
     2. n domains one fiber
     3. one domain n fiber
     4. n domain n fiber
  *)
  let gen_test_configs test_fn_lst =
    let cpu_cores = Domain.recommended_domain_count () in
    List.map
      (fun (n, gen, (t : 'a testcase)) ->
        List.map
          (fun (n_domains, n_fibers) ->
            let name =
              Printf.sprintf "%s (%d domains, %d fibers)" n n_domains n_fibers
            in
            Test.make ~print:Print.int ~name gen
              (testcase_wrapper @@ t ~n_domains ~n_fibers ~sched)
            |> QCheck_alcotest.to_alcotest)
          [ (1, 1); (cpu_cores, 1); (1, cpu_cores); (cpu_cores, cpu_cores) ])
      test_fn_lst
    |> List.flatten
  in
  gen_test_configs test_fn_lst

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs_threaded.enable ();
  Logs.set_level (Some Debug);
  Alcotest.run ~argv:Sys.argv "Obatcher"
    [
      ("Fifos scheduler", test_fn_all_for Fifos);
      ("Threaded scheduler", test_fn_all_for Threaded);
      ("Randos scheduler", test_fn_all_for Randos);
    ]
