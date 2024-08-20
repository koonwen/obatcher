open Uring

(* This backend submits requests eagerly but batches other requests
   that are waiting on completion of an earlier batch of requests *)
module Backend = struct
  type _ op =
    | Statx : (Unix.file_descr * Statx.Mask.t * Statx.t) -> int op
    | Read : (int * Unix.file_descr * Cstruct.t) -> int op
    | Write : (int * Unix.file_descr * Cstruct.t) -> int op

  type uid = int
  type wrapped_op = Mk : 'a op * 'a Picos.Computation.t -> wrapped_op

  type t = {
    ring : int Uring.t;
    pending : (uid, wrapped_op) Hashtbl.t;
    scheduled_retry : bool Atomic.t;
  }

  type cfg = { queue_depth : int }

  let get_uid =
    let cnt = ref 0 in
    fun () ->
      let id = !cnt in
      incr cnt;
      id

  let init ?(cfg = { queue_depth = 64 }) () =
    {
      ring = create ~queue_depth:cfg.queue_depth ();
      pending = Hashtbl.create 128;
      scheduled_retry = Atomic.make false;
    }

  (* Check and fill completions and schedule another fiber later to
     run this again if there are still pending completions *)
  let check_completions t =
    while Hashtbl.length t.pending > 0 do
      match get_cqe_nonblocking t.ring with
      | None -> Picos.Fiber.yield ()
      | Some { result; data = uid } ->
          (match Hashtbl.find t.pending uid with
          | Mk (Read _, comp) ->
              Logs.debug (fun m -> m "Read completion found");
              Picos.Computation.return comp result
          | Mk (Write _, comp) ->
              Logs.debug (fun m -> m "Write completion found");
              Picos.Computation.return comp result
          | Mk (Statx _, comp) ->
              Logs.debug (fun m -> m "Statx completion found");
              Picos.Computation.return comp result);
          Hashtbl.remove t.pending uid
    done

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
    (* Do batch submit, wait internally calls submit *)
    let submitted = Uring.submit t.ring in
    Logs.info (fun m -> m "Submitting %d requests as batch" submitted);
    check_completions t
end

include Obatcher.Make (Backend)

let read t file_offset fd buf =
  Logs.debug (fun m -> m "Requesting read");
  exec t (Read (file_offset, fd, buf))

let write t file_offset fd buf =
  Logs.debug (fun m -> m "Requesting write");
  exec t (Write (file_offset, fd, buf))

let statx t fd mask buf =
  Logs.debug (fun m -> m "Requesting statx");
  exec t (Statx (fd, mask, buf))
