open Utils

[@@@warning "-26"]

let btree_insert_sequential_threshold = ref None
let btree_search_sequential_threshold = ref None
let btree_search_parallel_threshold = ref None
let btree_max_children = ref 8

module Make (V : Map.OrderedType) = struct
  let ( .!() ) x v = Finite_vector.get x v

  module Sequential = struct
    type 'a node = {
      mutable n : int; (*  number of keys in node *)
      mutable keys : V.t Finite_vector.t; (* keys themselves *)
      mutable values : 'a Finite_vector.t; (* values *)
      leaf : bool;
      mutable children : 'a node Finite_vector.t;
      mutable no_elements : int;
          (* number of elements in the node and subtrees  *)
      mutable capacity : int;
      mutable min_child_capacity : int;
    }

    type 'a t = {
      mutable root : 'a node;
      mutable height : int;
      max_children : int;
    }

    let rec size_node node =
      if node.leaf then Finite_vector.length node.values
      else
        Finite_vector.fold_left
          (fun acc vl -> acc + size_node vl)
          0 node.children

    let size t = t.root.no_elements

    let rec pp_node ?(pp_child = true)
        ?(pp_v = fun fmt _ -> Format.fprintf fmt "<opaque>") indent f fmt node =
      let spaces = String.make indent ' ' in
      Format.fprintf fmt "%snode(n=%d,leaf=%b,no_elts=%d)\n%s - values=[%a]\n%a"
        spaces node.n node.leaf node.no_elements spaces
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (k, vl) -> Format.fprintf fmt "%a: %a" pp_v k f vl))
        (List.init node.n (fun i -> (node.keys.!(i), node.values.!(i))))
        (if pp_child then
           Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
             (fun fmt (k, vl) ->
               match k with
               | None ->
                   Format.fprintf fmt "%s - child(k=_):\n%a" spaces
                     (pp_node ~pp_v (indent + 4) f)
                     vl
               | Some key ->
                   Format.fprintf fmt "%s - child(k=%a):\n%a" spaces pp_v key
                     (pp_node ~pp_v (indent + 4) f)
                     vl)
         else fun _fmt _vl -> ())
        (List.init (Finite_vector.length node.children) (fun i ->
             ( (if i < node.n then Some node.keys.!(i) else None),
               node.children.!(i) )))

    let pp_node_internal = pp_node
    let pp_node ?pp_v f fmt vl = pp_node ?pp_v 0 f fmt vl
    let show_node ?pp_v f vl = Format.asprintf "%a" (pp_node ?pp_v f) vl

    let show_node_no_children ?pp_v f vl =
      Format.asprintf "%a" (pp_node_internal ?pp_v ~pp_child:false 0 f) vl

    let pp ?pp_v f fmt t = pp_node ?pp_v f fmt t.root
    let show ?pp_v f vl = Format.asprintf "%a" (pp ?pp_v f) vl

    let init ?max_children () =
      let max_children =
        match max_children with Some v -> v | None -> !btree_max_children
      in
      let root =
        {
          n = 0;
          leaf = true;
          keys = Finite_vector.init ~capacity:((2 * max_children) - 1) ();
          children = Finite_vector.init ~capacity:(2 * max_children) ();
          values = Finite_vector.init ~capacity:((2 * max_children) - 1) ();
          no_elements = 0;
          capacity = (2 * max_children) - 1;
          min_child_capacity = 0;
        }
      in
      { root; max_children; height = 1 }

    let rec fold_int_range ~start ~stop f acc =
      if start >= stop then f acc start
      else
        let acc = f acc start in
        fold_int_range ~start:(start + 1) ~stop f acc

    let rec find_int_range ~start ~stop f =
      if stop < start then None
      else if start = stop then f start
      else
        match f start with
        | None -> find_int_range ~start:(start + 1) ~stop f
        | res -> res

    let rec find_int_range_dec ~start ~stop f =
      if start < stop then None
      else if start = stop then f stop
      else
        match f start with
        | None -> find_int_range_dec ~start:(start - 1) ~stop f
        | res -> res

    let rec search_node x k =
      let index =
        find_int_range ~start:0 ~stop:(x.n - 1) (fun i ->
            if V.compare k x.keys.!(i) <= 0 then Some i else None)
        |> Option.value ~default:x.n
      in
      if index < x.n && V.compare x.keys.!(index) k = 0 then Some (x, index)
      else if x.leaf then None
      else search_node x.children.!(index) k

    let search t k =
      match search_node t.root k with
      | Some (node, i) -> Some node.values.!(i)
      | None -> None

    let min_capacity vec =
      Finite_vector.fold_left
        (fun acc vl ->
          match acc with
          | None -> Some vl.capacity
          | Some vl' when vl' > vl.capacity -> Some vl.capacity
          | _ -> acc)
        None vec

    (* pre: x.(i) has (2 * t - 1) keys *)
    let split_child x i =
      let y = x.children.!(i) in
      let t = (y.n + 1) / 2 in
      let z =
        let keys = Finite_vector.split_from y.keys t in
        let values = Finite_vector.split_from y.values t in
        let children =
          if y.leaf then Finite_vector.init ~capacity:(2 * t) ()
          else Finite_vector.split_from y.children t
        in
        let min_child_capacity =
          Option.value ~default:0 (min_capacity children)
        in
        let capacity = (t * (min_child_capacity + 1)) + min_child_capacity in
        {
          n = t - 1;
          leaf = y.leaf;
          keys;
          values;
          children;
          no_elements = t - 1;
          capacity;
          min_child_capacity;
        }
      in
      z.no_elements <- t - 1;
      Finite_vector.iter
        (fun child -> z.no_elements <- z.no_elements + child.no_elements)
        z.children;

      (* insert z *)
      Finite_vector.insert x.keys i y.keys.!(t - 1);
      Finite_vector.insert x.values i y.values.!(t - 1);
      Finite_vector.insert x.children (i + 1) z;

      (* clip y *)
      y.n <- t - 1;
      Finite_vector.clip y.keys (t - 1);
      Finite_vector.clip y.values (t - 1);
      y.no_elements <- t - 1;
      Finite_vector.iter
        (fun child -> y.no_elements <- y.no_elements + child.no_elements)
        y.children;
      y.min_child_capacity <- Option.value ~default:0 (min_capacity y.children);
      y.capacity <- (t * (y.min_child_capacity + 1)) + y.min_child_capacity;

      x.n <- x.n + 1;
      x.min_child_capacity <-
        min x.min_child_capacity (min y.min_child_capacity z.min_child_capacity);
      x.capacity <-
        (((2 * t) - 1 - x.n) * (x.min_child_capacity + 1))
        + x.min_child_capacity

    let rec insert_node ~max_children x k vl =
      let index =
        find_int_range_dec ~start:(x.n - 1) ~stop:0 (fun i ->
            if V.compare k x.keys.!(i) >= 0 then Some (i + 1) else None)
        |> Option.value ~default:0
      in
      x.no_elements <- x.no_elements + 1;
      if x.leaf then (
        Finite_vector.insert x.keys index k;
        Finite_vector.insert x.values index vl;
        x.capacity <- x.capacity - 1;
        x.n <- x.n + 1;
        x.capacity)
      else
        let child_capacity =
          if x.children.!(index).n = (2 * max_children) - 1 then (
            split_child x index;
            if V.compare k x.keys.!(index) > 0 then
              insert_node ~max_children x.children.!(index + 1) k vl
            else insert_node ~max_children x.children.!(index) k vl)
          else insert_node ~max_children x.children.!(index) k vl
        in
        x.min_child_capacity <- min x.min_child_capacity child_capacity;
        x.capacity <-
          (((2 * max_children) - 1 - x.n) * (x.min_child_capacity + 1))
          + x.min_child_capacity;
        x.capacity

    let insert tree k vl =
      let t = tree.max_children in
      let r = tree.root in
      if r.n = (2 * t) - 1 then (
        let s =
          {
            n = 0;
            leaf = false;
            keys = Finite_vector.init ~capacity:((2 * t) - 1) ();
            children = Finite_vector.singleton ~capacity:(2 * t) tree.root;
            values = Finite_vector.init ~capacity:((2 * t) - 1) ();
            no_elements = r.no_elements;
            capacity = 0;
            min_child_capacity = r.capacity;
          }
        in
        tree.root <- s;
        tree.height <- tree.height + 1;
        split_child s 0;
        ignore (insert_node ~max_children:tree.max_children s k vl))
      else ignore (insert_node ~max_children:tree.max_children r k vl)
  end

  module Batched = struct
    type 'a t = 'a Sequential.t
    type cfg = unit option

    type ('elt, 'ret) op =
      | Insert : V.t * 'elt -> ('elt, unit) op
      | Search : V.t -> ('elt, 'elt option) op
      | Size : ('elt, int) op

    type 'a wrapped_op =
      | Mk : ('a, 'b) op * 'b Picos.Computation.t -> 'a wrapped_op

    let init ?cfg:_ () =
      let max_children = !btree_max_children in
      Sequential.init ~max_children ()

    let fold_left_map f accu l =
      let rec aux accu l_accu = function
        | [] -> (accu, List.rev l_accu)
        | x :: l ->
            let accu, x = f accu x in
            aux accu (x :: l_accu) l
      in
      aux accu [] l

    let drop_last ls =
      let rec loop acc last = function
        | [] -> List.rev acc
        | h :: t -> loop (last :: acc) h t
      in
      match ls with [] -> [] | h :: t -> loop [] h t

    let int_pow x y =
      let rec loop acc x y =
        if y > 0 then
          match y mod 2 with
          | 0 -> loop acc (x * x) (y / 2)
          | _ -> loop (acc * x) x (y - 1)
        else acc
      in
      loop 1 x y

    let find_height ~t ~no_elts =
      if no_elts < (2 * t) - 1 then 1
      else
        let rec loop t no_elts h t_h t2_h =
          if t_h - 1 <= no_elts && no_elts <= t2_h - 1 then h
          else
            let t_h_1 = t_h * t and t2_h_1 = t2_h * (2 * t) in
            if t2_h - 1 < no_elts && no_elts < t2_h_1 - 1 then h + 1
            else loop t no_elts (h + 1) t_h_1 t2_h_1
        in
        loop t no_elts 1 t (2 * t)

    let find_split ?(root = false) ~t ~h r =
      let max_t = 2 * t in
      let min_size = int_pow t (h - 1) - 1 in
      let max_size = int_pow (2 * t) (h - 1) - 1 in
      let rec loop min_size max_size t =
        assert (t <= max_t);
        let elt_size = Int.div (r - t + 1) t in
        let rem_size = Int.rem (r - t + 1) t in
        if
          min_size <= elt_size && elt_size <= max_size
          && (rem_size = 0 || elt_size + 1 <= max_size)
        then (t, elt_size, rem_size)
        else loop min_size max_size (t + 1)
      in
      loop min_size max_size (if root then 2 else t)

    let partition_range ?root ~t ~h (start, stop) =
      let t, sub_range_size, rem = find_split ?root ~t ~h (stop - start) in
      let key_inds = Array.make (t - 1) 0 in
      let child_inds = Array.make t 0 in
      let rem = ref rem in
      let start = ref start in
      for i = 0 to t - 1 do
        let rem_comp =
          if !rem > 0 then (
            decr rem;
            1)
          else 0
        in
        child_inds.(i) <- min (!start + sub_range_size + rem_comp) stop;
        if i < t - 1 then key_inds.(i) <- !start + sub_range_size + rem_comp;
        start := !start + sub_range_size + rem_comp + 1
      done;
      child_inds.(t - 1) <- stop;
      (key_inds, child_inds)

    let rec build_node ~max_children:t ~h start stop arr =
      if h <= 1 then
        Sequential.
          {
            n = stop - start;
            keys =
              Finite_vector.init_with
                ~capacity:((2 * t) - 1)
                (stop - start)
                (fun i -> fst arr.(start + i));
            values =
              Finite_vector.init_with
                ~capacity:((2 * t) - 1)
                (stop - start)
                (fun i -> snd arr.(start + i));
            leaf = true;
            children = Finite_vector.init ~capacity:(2 * t) ();
            no_elements = stop - start;
            capacity = (2 * t) - 1 - (stop - start);
            min_child_capacity = 0;
          }
      else
        let key_inds, sub_ranges = partition_range ~t ~h (start, stop) in

        let children =
          let start = ref start in
          Array.map
            (fun stop ->
              let subtree =
                build_node ~max_children:t ~h:(h - 1) !start stop arr
              in
              start := stop + 1;
              subtree)
            sub_ranges
        in
        let n = Array.length key_inds in
        let keys =
          Finite_vector.init_with
            ~capacity:((2 * t) - 1)
            n
            (fun pos -> fst arr.(key_inds.(pos)))
        in
        let values =
          Finite_vector.init_with
            ~capacity:((2 * t) - 1)
            n
            (fun pos -> snd arr.(key_inds.(pos)))
        in
        let children =
          Finite_vector.init_with ~capacity:(2 * t) (Array.length children)
            (fun pos -> children.(pos))
        in
        let min_child_capacity =
          Sequential.min_capacity children |> Option.value ~default:0
        in
        let capacity = ((2 * t) - 1 - n) * (min_child_capacity + 1) in
        {
          n;
          keys;
          values;
          leaf = false;
          children;
          no_elements = stop - start;
          capacity;
          min_child_capacity;
        }

    let rec par_build_node ~max_children:t ~h start stop arr =
      if h <= 1 then
        Sequential.
          {
            n = stop - start;
            keys =
              Finite_vector.init_with
                ~capacity:((2 * t) - 1)
                (stop - start)
                (fun i -> fst arr.(start + i));
            values =
              Finite_vector.init_with
                ~capacity:((2 * t) - 1)
                (stop - start)
                (fun i -> snd arr.(start + i));
            leaf = true;
            children = Finite_vector.init ~capacity:(2 * t) ();
            no_elements = stop - start;
            capacity = (2 * t) - 1 - (stop - start);
            min_child_capacity = 0;
          }
      else
        let key_inds, sub_ranges = partition_range ~t ~h (start, stop) in

        let sub_ranges =
          let start = ref start in
          Array.map
            (fun stop ->
              let interval = (!start, stop) in
              start := stop + 1;
              interval)
            sub_ranges
        in
        let children =
          let child_arr =
            Array.make (Array.length sub_ranges)
              Sequential.
                {
                  n = 0;
                  children = Finite_vector.init ();
                  keys = Finite_vector.init ();
                  values = Finite_vector.init ();
                  leaf = true;
                  no_elements = 0;
                  capacity = 0;
                  min_child_capacity = 0;
                }
          in
          parallel_for ~start:0
            ~finish:(Array.length sub_ranges - 1)
            (fun i ->
              let start, stop = sub_ranges.(i) in
              child_arr.(i) <-
                par_build_node ~max_children:t ~h:(h - 1) start stop arr);
          child_arr
        in
        let n = Array.length key_inds in
        let keys =
          Finite_vector.init_with
            ~capacity:((2 * t) - 1)
            n
            (fun pos -> fst arr.(key_inds.(pos)))
        in
        let values =
          Finite_vector.init_with
            ~capacity:((2 * t) - 1)
            n
            (fun pos -> snd arr.(key_inds.(pos)))
        in
        let children =
          Finite_vector.init_with ~capacity:(2 * t) (Array.length children)
            (fun pos -> children.(pos))
        in
        let min_child_capacity =
          Sequential.min_capacity children |> Option.value ~default:0
        in
        let capacity =
          (((2 * t) - 1 - n) * (min_child_capacity + 1)) + min_child_capacity
        in
        {
          n;
          keys;
          values;
          leaf = false;
          children;
          no_elements = stop - start;
          capacity;
          min_child_capacity;
        }

    let build_from_sorted ?max_children:(t = 3) arr =
      let h = find_height ~t ~no_elts:(Array.length arr) in
      let root =
        if Array.length arr <= (2 * t) - 1 then
          par_build_node ~max_children:t ~h:1 0 (Array.length arr) arr
        else
          let key_inds, sub_ranges =
            partition_range ~root:true ~t ~h (0, Array.length arr)
          in

          let children =
            let start = ref 0 in
            Array.map
              (fun stop ->
                let subtree =
                  par_build_node ~max_children:t ~h:(h - 1) !start stop arr
                in
                start := stop + 1;
                subtree)
              sub_ranges
          in
          let n = Array.length key_inds in
          let keys =
            Finite_vector.init_with
              ~capacity:((2 * t) - 1)
              n
              (fun pos -> fst arr.(key_inds.(pos)))
          in
          let values =
            Finite_vector.init_with
              ~capacity:((2 * t) - 1)
              n
              (fun pos -> snd arr.(key_inds.(pos)))
          in
          let children =
            Finite_vector.init_with ~capacity:(2 * t) (Array.length children)
              (fun pos -> children.(pos))
          in
          let min_child_capacity =
            Sequential.min_capacity children |> Option.value ~default:0
          in
          let capacity =
            (((2 * t) - 1 - n) * (min_child_capacity + 1)) + min_child_capacity
          in
          {
            n;
            keys;
            values;
            leaf = false;
            children;
            no_elements = Array.length arr;
            min_child_capacity;
            capacity;
          }
      in
      (h, root)

    let rec int_range_downto start stop () =
      if start > stop then Seq.Nil
      else Seq.Cons (stop, int_range_downto start (stop - 1))

    let flatten t =
      let open Seq in
      let rec aux node =
        if node.Sequential.leaf then
          let elems =
            Array.init (Finite_vector.length node.Sequential.keys) (fun i ->
                (node.Sequential.keys.!(i), node.Sequential.values.!(i)))
          in
          Array.to_seq elems
        else
          let back =
            int_range_downto 1 node.Sequential.n
            |> fold_left
                 (fun acc i ->
                   let tl = aux node.Sequential.children.!(i) in
                   let kv =
                     ( node.Sequential.keys.!(i - 1),
                       node.Sequential.values.!(i - 1) )
                   in
                   let comb = cons kv tl in
                   append comb acc)
                 empty
          in
          append (aux node.Sequential.children.!(0)) back
      in
      aux t

    let merge i1 i2 =
      let i1 = Seq.to_dispenser i1 in
      let i2 = Seq.to_dispenser i2 in
      let next i h = match h with None -> i () | Some v -> Some v in
      let rec aux i1 h1 i2 h2 f =
        match (next i1 h1, next i2 h2) with
        | None, None -> ()
        | Some hd1, Some hd2 ->
            if hd1 < hd2 then (
              f hd1;
              aux i1 None i2 (Some hd2) f)
            else (
              f hd2;
              aux i1 (Some hd1) i2 None f)
        | Some hd1, None ->
            f hd1;
            aux i1 None i2 None f
        | None, Some hd2 ->
            f hd2;
            aux i1 None i2 None f
      in
      fun f -> aux i1 None i2 None f

    let par_rebuild ~max_children (root : 'a Sequential.node)
        (kv_arr : (V.t * 'a) array) =
      (* keys is a array of (key, index) where index is the position in the original search query *)
      let max_children = max_children in
      let batch =
        Array.make (Array.length kv_arr + root.no_elements) kv_arr.(0)
      in
      let i1 = kv_arr |> Array.to_seq in
      let i2 = flatten root in
      let merged = merge i1 i2 in
      let i = ref 0 in
      merged (fun vl ->
          batch.(!i) <- vl;
          incr i);
      build_from_sorted ~max_children batch

    let rec par_search_node ?(par_threshold = 6) ?(threshold = 64) node ~height
        ~(keys : (V.t * 'a option Picos.Computation.t) array)
        ~range:(rstart, rstop) =
      (* if the no elements in the node are greater than the number of keys we're searching for, then just do normal search in parallel *)
      let n = rstop - rstart in
      (* Format.printf "par_search batch_size is %d < threshold(%d), leaf?=%b, height=%d\n%!" *)
      (*   n threshold node.Sequential.leaf height; *)
      if n <= 0 then ()
      else if n = 1 then
        let k, kont = keys.(rstart) in
        Picos.Computation.return kont
          (Option.map
             (fun (node, i) -> node.Sequential.values.!(i))
             (Sequential.search_node node k))
      else if rstop - rstart < par_threshold && height > 5 then
        parallel_for ~start:rstart ~finish:(rstop - 1) (fun i ->
            let k, kont = keys.(i) in
            Picos.Computation.return kont
              (Option.map
                 (fun (node, i) -> node.Sequential.values.!(i))
                 (Sequential.search_node node k)))
      else if (rstop - rstart < threshold && height < 3) || node.Sequential.leaf
      then
        for i = rstart to rstop - 1 do
          let k, kont = keys.(i) in
          Picos.Computation.return kont
            (Option.map
               (fun (node, i) -> node.Sequential.values.!(i))
               (Sequential.search_node node k))
        done
      else
        let sub_intervals =
          Finite_vector.init ~capacity:(Finite_vector.length node.children) ()
        in
        let sub_interval_size i =
          let start, stop = sub_intervals.!(i) in
          stop - start
        in

        let last_sub_interval_end = ref rstart in
        (* partition batch by children  *)
        for i = 0 to Finite_vector.length node.keys - 1 do
          let interval_start = !last_sub_interval_end in
          while
            !last_sub_interval_end < rstop
            && V.compare (fst keys.(!last_sub_interval_end)) node.keys.!(i) < 0
          do
            incr last_sub_interval_end
          done;
          Finite_vector.insert sub_intervals i
            (interval_start, !last_sub_interval_end);
          while
            !last_sub_interval_end < rstop
            && V.compare (fst keys.(!last_sub_interval_end)) node.keys.!(i) = 0
          do
            Picos.Computation.return
              (snd keys.(!last_sub_interval_end))
              (Some node.values.!(i));
            incr last_sub_interval_end
          done
        done;
        Finite_vector.insert sub_intervals
          (Finite_vector.length node.keys)
          (!last_sub_interval_end, rstop);

        parallel_for ~start:0
          ~finish:(Finite_vector.length sub_intervals - 1)
          (fun i ->
            par_search_node ~par_threshold ~threshold node.children.!(i) ~keys
              ~height:(height - 1) ~range:sub_intervals.!(i))

    let par_search ?par_threshold ?threshold (t : 'a t)
        (keys : (V.t * 'a option Picos.Computation.t) array) =
      let threshold =
        match threshold with
        | Some _ -> threshold
        | None -> !btree_search_sequential_threshold
      in
      let par_threshold =
        match par_threshold with
        | Some _ -> par_threshold
        | None -> !btree_search_parallel_threshold
      in
      (* keys is a array of (key, index) where index is the position in the original search query *)
      Array.sort (fun (k, _) (k', _) -> V.compare k k') keys;
      (* allocate a buffer for the results *)
      par_search_node ?par_threshold ?threshold t.root ~height:t.height ~keys
        ~range:(0, Array.length keys)

    let rec par_insert_node ?(threshold = 8) ~max_children
        (t : 'a Sequential.node) (batch : (V.t * 'a) array) start stop =
      if stop <= start then t.min_child_capacity
      else if t.leaf || stop - start < threshold then (
        for i = start to stop - 1 do
          let key, vl = batch.(i) in
          ignore (Sequential.insert_node ~max_children t key vl)
        done;
        t.min_child_capacity)
      else (
        t.no_elements <- t.no_elements + stop - start;
        let sub_intervals =
          Finite_vector.init ~capacity:(2 * max_children) ()
        in
        let sub_interval_size i =
          let start, stop = sub_intervals.!(i) in
          stop - start
        in

        let last_sub_interval_end = ref start in

        (* partition batch by children  *)
        for i = 0 to Finite_vector.length t.keys - 1 do
          let interval_start = !last_sub_interval_end in
          while
            !last_sub_interval_end < stop
            && V.compare (fst batch.(!last_sub_interval_end)) t.keys.!(i) < 0
          do
            incr last_sub_interval_end
          done;
          Finite_vector.insert sub_intervals i
            (interval_start, !last_sub_interval_end)
        done;
        Finite_vector.insert sub_intervals
          (Finite_vector.length t.keys)
          (!last_sub_interval_end, stop);

        (* iterate through sub-intervals, and calculate number of splits that would be needed: *)
        let no_splits = ref 0 in
        for i = 0 to Finite_vector.length sub_intervals - 1 do
          if t.children.!(i).capacity < sub_interval_size i then incr no_splits
        done;
        (* now, as splitting children requires ownership of the whole node, we handle all splitting first *)
        let current_sub_interval = ref 0 in
        while !no_splits > 0 do
          (* skip over sub_intervals that are within capacity *)
          while
            t.children.!(!current_sub_interval).capacity
            >= sub_interval_size !current_sub_interval
          do
            incr current_sub_interval
          done;

          (* found a sub interval that is over-capacity *)
          while
            t.children.!(!current_sub_interval).capacity
            < sub_interval_size !current_sub_interval
          do
            let start, stop = sub_intervals.!(!current_sub_interval) in
            (* if current sub-interval's node is full *)
            if (2 * max_children) - 1 = t.children.!(!current_sub_interval).n
            then (
              (* split the child *)
              Sequential.split_child t !current_sub_interval;
              (* re-calculate interval, and interval for new child *)
              let new_stop_interval = ref start in
              while
                !new_stop_interval < stop
                && V.compare
                     (fst batch.(!new_stop_interval))
                     t.keys.!(!current_sub_interval)
                   < 0
              do
                incr new_stop_interval
              done;
              (* update intervals *)
              Finite_vector.set sub_intervals !current_sub_interval
                (start, !new_stop_interval);
              Finite_vector.insert sub_intervals
                (!current_sub_interval + 1)
                (!new_stop_interval, stop);
              (* update no splits: new child may also be over capacity *)
              if
                t.children.!(!current_sub_interval + 1).capacity
                < sub_interval_size (!current_sub_interval + 1)
              then incr no_splits)
            else if t.children.!(!current_sub_interval).capacity > 0 then (
              let to_insert = t.children.!(!current_sub_interval).capacity in
              let min_capacity =
                par_insert_node ~threshold ~max_children
                  t.children.!(!current_sub_interval)
                  batch start (start + to_insert)
              in
              Finite_vector.set sub_intervals !current_sub_interval
                (start + to_insert, stop);
              t.min_child_capacity <- min min_capacity t.min_child_capacity;
              t.capacity <-
                (((2 * max_children) - 1 - t.n) * (t.min_child_capacity + 1))
                + t.min_child_capacity)
            else
              let key, vl = batch.(start) in
              (* otherwise, just insert the current element *)
              let min_capacity =
                Sequential.insert_node ~max_children
                  t.children.!(!current_sub_interval)
                  key vl
              in
              (* update the interval to track the fact that the start no longer needs to be inserted *)
              Finite_vector.set sub_intervals !current_sub_interval
                (start + 1, stop);
              (* update capacity *)
              t.min_child_capacity <- min min_capacity t.min_child_capacity;
              t.capacity <-
                (((2 * max_children) - 1 - t.n) * (t.min_child_capacity + 1))
                + t.min_child_capacity
          done;
          (* we have successfully dispatched one of the splits *)
          decr no_splits
        done;

        (* now, all splits are done, do all in parallel! *)
        let min_child_capacity =
          parallel_for_reduce ~start:0
            ~finish:(Finite_vector.length sub_intervals - 1)
            ~body:(fun i ->
              let start, stop = sub_intervals.!(i) in
              par_insert_node ~max_children t.children.!(i) batch start stop)
            min t.min_child_capacity
        in
        t.min_child_capacity <- min t.min_child_capacity min_child_capacity;
        t.capacity <-
          (((2 * max_children) - 1 - t.n) * (t.min_child_capacity + 1))
          + t.min_child_capacity;

        t.capacity)

    let rec par_insert ?threshold (t : 'a t) (batch : (V.t * 'a) array) start
        stop =
      let n = stop - start in
      if n <= 0 (* a) we are finished inserting *) then ()
      else if t.root.leaf then (
        let key, vl = batch.(start) in
        Sequential.insert t key vl;
        par_insert ?threshold t batch (start + 1) stop)
      else if
        n <= t.root.capacity
        (* b) we are inserting fewer elements than our capacity - good! let's go! *)
      then
        ignore
          (par_insert_node ?threshold ~max_children:t.max_children t.root batch
             start stop)
      else if
        (2 * t.max_children) - 1
        = t.root.n (* c) our root has reached max capacity - split! *)
      then (
        let s =
          Sequential.
            {
              n = 0;
              leaf = false;
              keys = Finite_vector.init ~capacity:((2 * t.max_children) - 1) ();
              children =
                Finite_vector.singleton ~capacity:(2 * t.max_children) t.root;
              values =
                Finite_vector.init ~capacity:((2 * t.max_children) - 1) ();
              no_elements = t.root.no_elements;
              capacity = 0;
              min_child_capacity = t.root.min_child_capacity;
            }
        in
        t.root <- s;
        t.height <- t.height + 1;
        Sequential.split_child s 0;
        par_insert ?threshold t batch start stop)
      else if n = 1 then
        ignore
          (Sequential.insert_node ~max_children:t.max_children t.root
             (fst batch.(start))
             (snd batch.(start)))
      else (
        (* d) insert as much as we can and repeat! *)
        assert (t.root.capacity > 0);
        let capacity = t.root.capacity in
        ignore
          (par_insert_node ?threshold ~max_children:t.max_children t.root batch
             start (start + capacity));
        par_insert ?threshold t batch (start + capacity) stop)

    let par_insert ?threshold ?(can_rebuild = true) t batch =
      let threshold =
        match threshold with
        | Some _ -> threshold
        | None -> !btree_insert_sequential_threshold
      in
      if
        Array.length batch > 0
        && Array.length batch > t.Sequential.root.no_elements
        && can_rebuild
      then (
        let height, root =
          par_rebuild ~max_children:t.Sequential.max_children t.root batch
        in
        t.Sequential.root <- root;
        t.Sequential.height <- height)
      else par_insert ?threshold t batch 0 (Array.length batch)

    let run (type a) (t : a t) (ops : a wrapped_op array) : unit =
      let searches : (V.t * a option Picos.Computation.t) list ref = ref [] in
      let inserts : (V.t * a) list ref = ref [] in
      let start_size = t.root.no_elements in
      Array.iter
        (fun (elt : a wrapped_op) ->
          match elt with
          | Mk (Insert (key, vl), kont) ->
              Picos.Computation.return kont ();
              inserts := (key, vl) :: !inserts
          | Mk (Search key, kont) -> searches := (key, kont) :: !searches
          | Mk (Size, kont) -> Picos.Computation.return kont start_size)
        ops;
      let searches = Array.of_list !searches in
      if Array.length searches > 0 then par_search t searches;
      let inserts = Array.of_list !inserts in
      if Array.length inserts > 0 then (
        Array.sort (fun (k1, _) (k2, _) -> V.compare k1 k2) inserts;
        par_insert t inserts)
  end

  include Obatcher.Make_Poly (Batched)

  let insert t k v = exec t (Batched.Insert (k, v))
  let search t v = exec t (Batched.Search v)
  let size t = exec t Batched.Size
end
