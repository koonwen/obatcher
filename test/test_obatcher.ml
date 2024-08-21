[@@@warning "-37-32-21"]

open QCheck2

module Mock_Service_Internal = struct
  type t = int array Queue.t
  (* Collect all batches
     seen for test
     inspection *)

  type cfg
  type 'a op = Req : int -> unit op
  type wrapped_op = Mk : 'a op * 'a Picos.Computation.t -> wrapped_op

  let init ?cfg:_ () = Queue.create ()

  let run t ops =
    let ids =
      Array.map
        (function
          | Mk (Req id, comp) ->
              Picos.Computation.return comp ();
              id)
        ops
    in
    Queue.add ids t

  let get_batches (t : t) = Queue.to_seq t |> Array.of_seq
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
  for i = 1 to n do
    Mock_Service.exec t (Mock_Service_Internal.Req i)
  done

(* Split n_req to be run in n fibers *)
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

(* Setup f to run with n_domains and specified scheduler *)
let run_with ~sched ~n_domains ~f =
  let n_domains_to_spawn = n_domains - 1 in
  match sched with
  | Threaded ->
      let domains =
        List.init n_domains_to_spawn (fun _ ->
            Domain.spawn (fun () -> Picos_threaded.run f))
      in
      List.iter Domain.join domains
  | Fifos ->
      let domains =
        List.init n_domains_to_spawn (fun _ ->
            Domain.spawn (fun () -> Picos_fifos.run f))
      in
      List.iter Domain.join domains
  | Randos ->
      let context = Picos_randos.context () in
      let domains =
        List.init n_domains_to_spawn (fun _ ->
            Domain.spawn (fun () -> Picos_randos.runner_on_this_thread context))
      in
      Picos_randos.run ~context f;
      List.iter Domain.join domains

let run_submissions_then_get_batches_with ~sched ~n_domains ~n_fibers ~n_req =
  let mock_service = Mock_Service.init () in
  run_with ~sched ~n_domains ~f:(fun () ->
      submit_f mock_service ~n_fibers ~n_req);
  let internal = Mock_Service.get_internal mock_service in
  Mock_Service_Internal.get_batches internal

(* Input generators *)
let small_nat_nonzero =
  let open Gen in
  small_nat >|= fun i -> i + 1

(* Test config *)
type 'a test_cfg =
  sched:scheduler -> n_domains:int -> n_fibers:int -> 'a -> bool

(* Test functions *)
let test_fn_n_req_seen ~sched ~n_domains ~n_fibers n_req =
  assume (n_req >= n_fibers);
  let batches_seen =
    run_submissions_then_get_batches_with ~sched ~n_domains ~n_fibers ~n_req
  in
  let n_req_seen =
    Array.fold_left (fun acc batch -> Array.length batch + acc) 0 batches_seen
  in
  Printf.printf "Got n_req_seen %d\n" n_req_seen;
  n_req_seen = n_req

let test_fn_lst = [ ("n_req_seen", small_nat_nonzero, test_fn_n_req_seen) ]

let test_fn_all_for sched =
  (* Generate each test for 4 different configurations
     1. one domain one fiber
     2. n domains one fiber
     3. one domain n fiber
     4. n domain n fiber
  *)
  let gen_test_configs test_fn_lst =
    let cpu_cores = Domain.recommended_domain_count () in
    List.map
      (fun (n, gen, (t : 'a test_cfg)) ->
        List.map
          (fun (n_domains, n_fibers) ->
            let name =
              Printf.sprintf "%s (%d domains, %d fibers)" n n_domains n_fibers
            in
            Test.make ~print:Print.int ~name gen (t ~n_domains ~n_fibers ~sched)
            |> QCheck_alcotest.to_alcotest)
          [ (1, 1); (cpu_cores, 1); (1, cpu_cores); (cpu_cores, cpu_cores) ])
      test_fn_lst
    |> List.flatten
  in
  gen_test_configs test_fn_lst

let () =
  Alcotest.run "Obatcher"
    [
      ("Fifos scheduler", test_fn_all_for Randos);
      ("Threaded scheduler", test_fn_all_for Threaded);
      ("Randos scheduler", test_fn_all_for Randos);
    ]

let strange_behviour () =
  let show_tid () = Printf.printf "ID %d\n" (Thread.id (Thread.self ())) in
  show_tid ();
  let domains =
    List.init 4 (fun _ -> Domain.spawn (fun () -> Picos_fifos.run ignore))
  in
  assert (Domain.is_main_domain ());
  show_tid ();
  List.iter Domain.join domains
