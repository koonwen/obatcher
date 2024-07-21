
open Uring
open! Picos

module Backend = struct
  type _ op =
    | Statx : (Unix.file_descr * Statx.Mask.t * Statx.t) -> int op
    | Read : (int * Unix.file_descr * Cstruct.t) -> int op
    | Write : (int * Unix.file_descr * Cstruct.t) -> int op

  type uid = int

  let get_uid =
    let cnt = ref 0 in
    fun () ->
      let id = !cnt in
      incr cnt;
      id

  type wrapped_op = Mk : 'a op * 'a Picos.Computation.t -> wrapped_op

  type t = {
    ring : int Uring.t;
    pending : (uid, wrapped_op) Hashtbl.t;
    scheduled_retry : bool Atomic.t;
  }

  (* Check and fill completions and schedule another fiber later to
     run this again if there are still pending completions *)
  let rec check_completions t =
    let has_comp = ref true in
    while !has_comp do
      match get_cqe_nonblocking t.ring with
      | None -> has_comp := false
      | Some { result; data = uid } ->
          (match Hashtbl.find t.pending uid with
          | Mk (Read _, comp) ->
              Printf.printf "Read completion found\n%!";
              Picos.Computation.return comp result
          | Mk (Write _, comp) ->
              Printf.printf "Write completion found\n%!";
              Picos.Computation.return comp result
          | Mk (Statx _, comp) ->
              Printf.printf "Statx completion found\n%!";
              Picos.Computation.return comp result);
          Hashtbl.remove t.pending uid
    done;
    (* Schedule next try again *)
    let pending = Hashtbl.length t.pending in
    if pending > 0 then (
      Printf.printf "Pending %d completions, scheduling retry\n%!" pending;
      (* yield and retry later *)
      Picos.Fiber.yield ();
      check_completions t)

  let run (t : t) batch =
    (* Submit requests to uring *)
    Array.iter
      (function
        | Mk (Read (i, fd, buf), _) as m ->
            let uid = get_uid () in
            Hashtbl.add t.pending uid m;
            read t.ring ~file_offset:(Optint.Int63.of_int i) fd buf uid
            |> ignore
        | Mk (Write (i, fd, buf), _) as m ->
            let uid = get_uid () in
            Hashtbl.add t.pending uid m;
            write t.ring ~file_offset:(Optint.Int63.of_int i) fd buf uid
            |> ignore
        | Mk (Statx (fd, mask, buf), _) as m ->
            let uid = get_uid () in
            Hashtbl.add t.pending uid m;
            statx t.ring ~fd ~mask "" buf Statx.Flags.empty_path uid |> ignore)
      batch;
    (* Do batch submit *)
    Printf.printf "Submitted %d requests as batch\n%!" (Uring.submit t.ring);
    check_completions t

  let init ~ctx:_ =
    {
      ring = create ~queue_depth:64 ();
      pending = Hashtbl.create 128;
      scheduled_retry = Atomic.make false;
    }
end

module Uring_backend = struct
  open Obatcher.Make (Backend)

  let init () = init ~ctx:()

  let read t file_offset fd buf =
    Printf.printf "Requesting read\n%!";
    apply t (Read (file_offset, fd, buf))

  let write t file_offset fd buf =
    Printf.printf "Requesting write\n%!";
    apply t (Write (file_offset, fd, buf))

  let statx t fd mask buf =
    Printf.printf "Requesting statx\n%!";
    apply t (Statx (fd, mask, buf))
end
[@@warning "-32"]

let copy backend filepath dst_filepath =
  (* let exception EOF in *)
  let fd_from = UnixLabels.openfile filepath ~mode:[ O_RDONLY ] ~perm:0 in
  let fd_to =
    UnixLabels.openfile dst_filepath
      ~mode:[ O_CREAT; O_TRUNC; O_WRONLY ]
      ~perm:0o644
  in
  let bufsz = 16000000 in
  let buf = Cstruct.create bufsz in
  let statxbuf = Statx.create () in
  Uring_backend.statx backend fd_from Statx.Mask.size statxbuf |> ignore;
  let iter = ((Statx.size statxbuf |> Int64.to_int) / bufsz) + 1 in
  Picos_structured.Run.all
    (List.init iter (fun i () ->
           let read = Uring_backend.read backend (i * bufsz) fd_from buf in
           (* Printf.printf "Read %d bytes\n%!" read; *)
           (* if read = 0 then raise EOF; *)
           let buf = Cstruct.sub buf 0 read in
           let wrote = Uring_backend.write backend (i * bufsz) fd_to buf in
           (* Printf.printf "Wrote %d bytes\n%!" wrote; *)
           assert (read = wrote)))

let () =
  let open Uring_backend in
  let backend = init () in
  Picos_fifos.run (fun () ->
      Picos_structured.Run.all
        [
          (fun () ->
            Printf.printf "Fiber 1%!";
            copy backend "1gb.file" "1gb.copy1");
          (* (fun () -> *)
          (*   Printf.printf "Fiber 2%!"; *)
          (*   copy backend "1gb.file" "1gb.copy2"); *)
          (* (fun () -> *)
          (*   Printf.printf "Fiber 3%!"; *)
          (*   copy backend "1gb.file" "1gb.copy3"); *)
        ])
