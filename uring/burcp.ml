open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

type scheduler = Threaded | Fifos | Randos

let scheduler_conv =
  let parser = function
    | "threaded" -> Ok Threaded
    | "fifos" -> Ok Fifos
    | "randos" -> Ok Randos
    | _ -> Error (`Msg "Unknown option, please provide one of <threaded | fifos | randos>")
  in
  let printer ppf = function
    | Threaded -> Format.fprintf ppf "Threaded"
    | Fifos -> Format.fprintf ppf "Fifos"
    | Randos -> Format.fprintf ppf "Randos" in
  Arg.conv (parser, printer)

let copy backend infile outfile () =
  let open Uring in
  let fd_from = UnixLabels.openfile infile ~mode:[ O_RDONLY ] ~perm:0 in
  let fd_to =
    UnixLabels.openfile outfile ~mode:[ O_CREAT; O_TRUNC; O_WRONLY ] ~perm:0o644
  in
  let bufsz = 4096 in
  let buf = Cstruct.create bufsz in
  let statxbuf = Statx.create () in
  Batched_uring.statx backend fd_from Statx.Mask.size statxbuf |> ignore;
  let iter = ((Statx.size statxbuf |> Int64.to_int) / bufsz) + 1 in
  Picos_structured.Run.all
    (List.init iter (fun i () ->
         let read = Batched_uring.read backend (i * bufsz) fd_from buf in
         let buf = Cstruct.sub buf 0 read in
         let wrote = Batched_uring.write backend (i * bufsz) fd_to buf in
         assert (read = wrote)))

let run scheduler infile outfile () =
  let backend = Batched_uring.init () in
  match scheduler with
  | Threaded -> Picos_threaded.run (copy backend infile outfile)
  | Fifos -> Picos_fifos.run (copy backend infile outfile)
  | Randos -> Picos_randos.run (copy backend infile outfile)

let cmd =
  let setup_log =
    Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
  in
  let infile =
    let doc = "Source filename to copy from" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"SOURCE_FILE" ~doc)
  in
  let outfile =
    let doc = "Target filename to copy to" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"TARGET_FILE" ~doc)
  in
  let scheduler =
    let doc = "Type of scheduler to use" in
    Arg.(value & opt scheduler_conv Fifos & info [ "s" ] ~docv:"SCHED" ~doc)
  in
  let doc = "copy a file using batched interface over io_uring" in
  let info = Cmd.info "burcp" ~doc in
  Cmd.v info Term.(const run $ scheduler $ infile $ outfile $ setup_log)

let () =
  match Cmd.eval cmd with
  | 0 -> exit (if Logs.err_count () > 0 then 1 else 0)
  | _ -> exit 1
