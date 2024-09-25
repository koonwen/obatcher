[@@@alert "-unsafe"]

open Containers
open QCheck

module Test_skiplist = struct
  module Isl = Ds.Batched_skiplist.Make (Int)

  let test_insert =
    QCheck.Test.make ~count:1000 ~name:"inserts return true on member lookup"
      (QCheck.list small_int) (fun ins_l ->
        Picos_mux_random.run @@ fun () ->
        let t = Isl.init () in
        (* Insert all elements in l1 *)
        List.iter (Isl.insert t) ins_l;
        List.for_all (fun v -> Isl.mem t v) ins_l)

  let test_mem =
    QCheck.Test.make ~count:1000 ~name:"member lookup not vacuously true"
      (QCheck.list_of_size (Gen.int_range 50 100) small_int)
      (fun l ->
        let l' = List.uniq ~eq:(fun i1 i2 -> Int.compare i1 i2 = 0) l in
        let len = List.length l' in
        let ins_l, mem_l = List.take_drop (len / 2) l' in
        Picos_mux_random.run @@ fun () ->
        let t = Isl.init () in
        (* Insert all elements in l1 *)
        List.iter (Isl.insert t) ins_l;
        List.for_all (fun v -> Bool.equal (Isl.mem t v) false) mem_l
        && (not (List.is_empty ins_l))
        && not (List.is_empty mem_l))

  let test_size =
    QCheck.Test.make ~count:1000 ~name:"duplicate elements are not inserted"
      (QCheck.list_of_size (Gen.int_range 52 100) (int_range (-25) 25))
      (fun ins_l ->
        let l' = List.uniq ~eq:(fun i1 i2 -> Int.compare i1 i2 = 0) ins_l in
        let len = List.length l' in
        Picos_mux_random.run @@ fun () ->
        let t = Isl.init () in
        (* Insert all elements in l1 *)
        List.iter (Isl.insert t) ins_l;
        Isl.sz t = len && len <> List.length ins_l)

  let suite =
    List.map QCheck_alcotest.to_alcotest [ test_insert; test_mem; test_size ]
end

module Test_btree = struct
  module Ibtree = Ds.Batched_btree.Make (Int)

  let test_insert =
    QCheck.Test.make ~count:1000
      ~name:"inserts return true & correct element on member lookup"
      (QCheck.list small_int) (fun ins_l ->
        Picos_mux_random.run @@ fun () ->
        let t = Ibtree.init () in
        (* Insert all elements in l1 *)
        List.iter (fun v -> Ibtree.insert t v v) ins_l;
        List.for_all
          (fun v ->
            match Ibtree.search t v with None -> false | Some v' -> v = v')
          ins_l)

  let test_mem =
    QCheck.Test.make ~count:1000 ~name:"member lookup not vacuously true"
      (QCheck.list_of_size (Gen.int_range 50 100) small_int)
      (fun l ->
        let l' = List.uniq ~eq:(fun i1 i2 -> Int.compare i1 i2 = 0) l in
        let len = List.length l' in
        let ins_l, mem_l = List.take_drop (len / 2) l' in
        Picos_mux_random.run @@ fun () ->
        let t = Ibtree.init () in
        (* Insert all elements in l1 *)
        List.iter (fun v -> Ibtree.insert t v v) ins_l;
        List.for_all (fun v -> Option.is_none (Ibtree.search t v)) mem_l
        && (not (List.is_empty ins_l))
        && not (List.is_empty mem_l))

  let test_size =
    QCheck.Test.make ~if_assumptions_fail:(`Fatal, 1.0) ~count:1000
      ~name:"size = elements inserted, duplicates persist"
      (QCheck.list_of_size (Gen.int_range 52 100) (int_range (-25) 25))
      (fun ins_l ->
        let len = List.length ins_l in
        let len_uniq =
          List.length (List.uniq ~eq:(fun i1 i2 -> Int.compare i1 i2 = 0) ins_l)
        in
        assume (len_uniq <> len);
        Picos_mux_random.run @@ fun () ->
        let t = Ibtree.init () in
        (* Insert all elements in l1 *)
        List.iter (fun v -> Ibtree.insert t v v) ins_l;
        Ibtree.size t = len)

  (* Behaviour for duplicate keys are undefined *)
  let _test_duplicate_key =
    QCheck.Test.make ~count:1000 ~name:"test duplicate key insert updates value"
      (QCheck.list_of_size Gen.nat (tup2 int int))
      (fun kv_l ->
        assume (List.length kv_l > 0);
        Picos_mux_random.run @@ fun () ->
        let t = Ibtree.init () in
        (* Insert all elements in l1 *)
        List.iter (fun (k, v) -> Ibtree.insert t k v) kv_l;
        let k, _ = List.hd kv_l in
        let updates_overwrite =
          List.for_all
            (fun (_, v) ->
              Ibtree.insert t k v;
              match Ibtree.search t k with
              | None -> false
              | Some v' ->
                  if v = v' then true
                  else
                    let t_internal = Ibtree.get_internal t in
                    Test.fail_reportf "Search key %d, Expected %d Got %d\n%s" k
                      v v'
                      (Ibtree.Sequential.show ~pp_v:Format.pp_print_int
                         Format.pp_print_int t_internal))
            kv_l
        in
        let expected_sz = List.length kv_l * 2 in
        let sz = Ibtree.size t in
        let duplicate_kv_persist = sz = expected_sz in
        if updates_overwrite && duplicate_kv_persist then true
        else
          let t_internal = Ibtree.get_internal t in
          Test.fail_reportf "Size expected %d Got %d\n%s" expected_sz sz
            (Ibtree.Sequential.show ~pp_v:Format.pp_print_int
               Format.pp_print_int t_internal))

  let suite =
    List.map QCheck_alcotest.to_alcotest [ test_insert; test_mem; test_size ]
end

let () =
  Alcotest.run "Batched data structures"
    [ ("Skiplist", Test_skiplist.suite); ("Btree", Test_btree.suite) ]
