module ParSort = struct
  module T = Obatcher.Task

  let bubble_sort_threshold = 32

  let bubble_sort ~compare (a : 'a array) start limit =
    for i = start to limit - 2 do
      for j = i + 1 to limit - 1 do
        if compare a.(j) a.(i)  < 0 then
          let t = a.(i) in
          a.(i) <- a.(j);
          a.(j) <- t;
      done
    done

  let merge ~compare (src : 'a array) dst start split limit =
    let rec loop dst_pos i j =
      if i = split then
        Array.blit src j dst dst_pos (limit - j)
      else if j = limit then
        Array.blit src i dst dst_pos (split - i)
      else if compare src.(i) src.(j) <= 0 then begin
        dst.(dst_pos) <- src.(i);
        loop (dst_pos + 1) (i + 1) j;
      end else begin
        dst.(dst_pos) <- src.(j);
        loop (dst_pos + 1) i (j + 1);
      end in
    loop start start split

  let rec merge_sort pool ~compare move a b start limit =
    if move || limit - start > bubble_sort_threshold then
      let split = (start + limit) / 2 in
      let r1 = T.async pool (fun () -> merge_sort pool ~compare (not move) a b start split) in
      let r2 = T.async pool (fun () -> merge_sort pool ~compare (not move) a b split limit) in
      T.await pool r1;
      T.await pool r2;
      if move then merge ~compare a b start split limit else merge ~compare b a start split limit
    else bubble_sort ~compare a start limit

  let sort pool ~compare a =
    let b = Array.copy a in
    merge_sort pool ~compare false a b 0 (Array.length a)
end

module Finite_vector = struct
  type 'a data =
    | Empty of int
    | Buf of 'a array

  let capacity = function Empty n -> n | Buf a -> Array.length a

  type 'a t = { mutable size: int; mutable buf: 'a data }

  let length t = t.size

  let pp f fmt t =
    match t.buf with
    | Empty cap -> Format.fprintf fmt "[| %s |]" (String.concat "; " (List.init cap (fun _ -> "_")))
    | Buf arr ->
      Format.fprintf fmt "[| %a |]"
        (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt "; ")
           (fun fmt -> function None -> Format.fprintf fmt "_"
                              | Some vl -> f fmt vl))
        (List.init (Array.length arr) (fun i -> if i < t.size then Some arr.(i) else None))

  let init ?(capacity=8) () = {size=0; buf=Empty capacity}

  let init_with ?(capacity=8) n f =
    let capacity = max n capacity in
    let n = max n 0 in
    if n = 0 then {size=0; buf=Empty capacity}
    else
      let saved = ref None in
      let arr = Array.init capacity (fun i ->
          if i = n - 1
          then (let res = f i in saved := Some res; res)
          else if i < n
          then f i
          else Option.get !saved
        ) in
      {size=n; buf=Buf arr}

  let singleton ?(capacity=8) v = {size=1; buf=Buf (Array.make capacity v)}

  let to_array t = match t.buf with Empty _ -> [| |] | Buf a -> Array.sub a 0 t.size

  let get t i =
    if t.size <= i then
      invalid_arg "invalid index for dereference";
    match t.buf with
    | Empty _ -> failwith "found empty buf"
    | Buf arr -> arr.(i)

  let set t i vl =
    if t.size <= i then
      invalid_arg "invalid index for dereference";
    match t.buf with
    | Empty _ -> failwith "found empty buf"
    | Buf arr -> arr.(i) <- vl


  let fold_left f x a =
    match a.buf with
    | Empty _ -> x
    | Buf arr ->
      let r = ref x in
      for i = 0 to a.size - 1 do
        r := f !r (Array.unsafe_get arr i)
      done;
      !r

  let iter f a =
    match a.buf with
    | Empty _ -> ()
    | Buf arr ->
      for i = 0 to a.size - 1 do
        f (Array.unsafe_get arr i)
      done

  let split_from t index =
    if t.size < index || index < 0 then
      invalid_arg "splitting by invalid index";
    match t.buf with
    | Empty n -> {size=0; buf=Empty n}
    | Buf arr ->
      let new_arr = (Array.init (Array.length arr) (fun i ->
          if index + i < t.size
          then arr.(index + i)
          else arr.(t.size - 1)
        )) in
      let upper_buffer = {
        size=(t.size - index);
        buf=Buf new_arr
      } in
      t.size <- index;
      upper_buffer

  let drop_last t =
    if t.size <= 0 then
      invalid_arg "attempt to drop last on empty array";
    if t.size > 1 then begin
      match t.buf with
      | Empty _ -> assert false
      | Buf arr ->
        arr.(t.size - 1) <- arr.(t.size - 2)
    end;
    t.size <- t.size - 1

  let insert t i vl =
    if t.size >= capacity t.buf then
      failwith "out of capacity";
    if i >= t.size + 1 then
      invalid_arg "invalid index for insert";
    match t.buf with
    | Empty cap ->
      let arr = Array.make cap vl in
      t.size <- i + 1;
      t.buf <- Buf arr
    | Buf arr ->
      for j = t.size downto i + 1 do
        arr.(j) <- arr.(j - 1);
      done;
      t.size <- t.size + 1;
      arr.(i) <- vl

  let clip t i =
    if i > t.size then
      invalid_arg "attempt to clip larger than size";
    if i < 0 then
      invalid_arg "invalid clip size less than 0";
    match t.buf with
    | Empty _ -> ()
    | Buf arr ->
      if i > 0 then begin
        for j = i to t.size do
          arr.(j) <- arr.(j-1)
        done;
        t.size <- i
      end else begin
        t.buf <- Empty (Array.length arr);
        t.size <- 0
      end

end
