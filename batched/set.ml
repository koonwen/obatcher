(* Tree implementing a set *)
module Tree_set = struct
  module Make (Ord : Stdlib.Set.OrderedType) = struct
    type elt = Ord.t
    type tree = Lf | Node of tree * elt * tree
    type t = tree ref

    let get_val = function Lf -> failwith "Not a Node" | Node (_, v, _) -> v

    let insert_node t node =
      let node_v = get_val node in
      let rec aux = function
        | Lf -> node
        | Node (left, cur_v, right) as n -> (
            match Ord.compare node_v cur_v with
            | 0 -> n (* Value already in tree *)
            | v -> if v < 0 then aux left else aux right)
      in
      t := aux !t

    let mem t find_v =
      let rec aux = function
        | Lf -> false
        | Node (left, cur_v, right) -> (
            match Ord.compare find_v cur_v with
            | 0 -> true
            | v -> if v < 0 then aux left else aux right)
      in
      aux !t

    let fold_right f acc t =
      let rec aux acc = function
        | Lf -> acc
        | Node (left, cur_v, right) ->
            let r_acc = aux acc right in
            let cur_acc = f r_acc cur_v in
            aux cur_acc left
      in
      aux acc t

    let to_list t = fold_right (fun acc elt -> elt :: acc) [] t

    let partition_n n l =
      let rec aux n = function
        | [] -> ([], [])
        | h :: t as lst ->
            if n = 0 then ([], lst)
            else
              let left, right = aux (n - 1) t in
              (h :: left, right)
      in
      aux n l

    let pivot l : elt list * elt * elt list =
      let len = List.length l in
      (* Floor division *)
      let mid = len / 2 in
      let left, right = partition_n mid l in
      (left, List.hd right, List.tl right)

    let rebuild t lst =
      let rec aux = function
        | [] -> Lf
        | l ->
            let left, cur_v, right = pivot l in
            Node (aux left, cur_v, aux right)
      in
      t := aux lst

    (* Implement deletion *)
  end
end

module Make (Ord : Stdlib.Set.OrderedType) = struct
  module Tree = Tree_set.Make (Ord)

  type t = Tree.t

  type 'a op =
    | Del : Ord.t -> unit op
    | Mem : Ord.t -> bool op
    | Ins : Ord.t -> unit op

  type wrapped_op = Mk : 'a op * 'a Picos.Computation.t -> wrapped_op
  let wrop_prio = function
    | Mk (Del _, _) -> 0
    | Mk (Mem _, _) -> 1
    | Mk (Ins _, _) -> 2

  let compare wrop1 wrop2 = Int.compare (wrop_prio wrop1) (wrop_prio wrop2)

  let init ~ctx:_ = ref Tree.Lf

  let preprocess _batch = failwith "Not implemented"

  let run t batch =
    Array.sort compare batch;
    Array.iter
      (function
        | Mk (Del _, _comp) -> failwith "Not implemented"
        | Mk (Ins _, comp) -> Picos.Computation.return comp ()
        | Mk (Mem v, comp) -> Picos.Computation.return comp (Tree.mem t v))
      batch
end
