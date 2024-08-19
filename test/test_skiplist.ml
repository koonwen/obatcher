open Containers
open QCheck
module Isl = Ds.Skiplist.Make (Int)

let test_insert =
  QCheck.Test.make ~count:1000 ~name:"inserts return true on member lookup"
    (QCheck.list small_int) (fun ins_l ->
      Picos_fifos.run @@ fun () ->
      let t = Isl.init ~ctx:() in
      (* Insert all elements in l1 *)
      List.iter (Isl.insert t) ins_l;
      List.for_all (fun v -> Isl.mem t v) ins_l)

let test_mem =
  QCheck.Test.make ~count:1000 ~name:"member lookup not vacuously true"
    (QCheck.list_of_size (Gen.int_range 50 100) small_int) (fun l ->
      let l' = List.uniq ~eq:(fun i1 i2 -> Int.compare i1 i2 = 0) l in
      let len = List.length l' in
      let ins_l, mem_l = List.take_drop (len / 2) l' in
      Picos_fifos.run @@ fun () ->
      let t = Isl.init ~ctx:() in
      (* Insert all elements in l1 *)
      List.iter (Isl.insert t) ins_l;
      List.for_all (fun v -> Bool.equal (Isl.mem t v) false) mem_l
      && (not (List.is_empty ins_l))
      && not (List.is_empty mem_l))

let () =
  let suite = List.map QCheck_alcotest.to_alcotest [ test_insert; test_mem ] in
  Alcotest.run "Batched data structures" [ ("skiplist", suite) ]
