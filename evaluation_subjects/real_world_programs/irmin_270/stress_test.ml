open Lwt.Infix

let log_on = false

let return = Lwt.return
let print = Printf.printf

let (/) = Filename.concat

let tmpdir = "." / "irmin-watcher"

let total_watches = ref 5
let watches_started = ref 0
let watches_wrote = ref 0

let clean () = if Sys.file_exists tmpdir then (
    ignore @@ Sys.command (Printf.sprintf "rm -rf '%s'" tmpdir)
  )

let run f =
  clean ();
  Lwt_main.run (Lwt.pick[f (); Lwt_unix.sleep 0.1])

let rec mkdir d =
  let perm = 0o0700 in
  try Unix.mkdir d perm
  with
  | Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> ()
  | Unix.Unix_error (Unix.ENOENT, "mkdir", _) ->
      mkdir (Filename.dirname d);
      Unix.mkdir d perm

let write f d  i =
  let f = tmpdir / f in
  mkdir (Filename.dirname f);
  Lwt_io.open_file Lwt_io.Output f >>= fun ch ->
  Lwt_io.write ch d >>= fun () ->
  Lwt_io.close ch


let move a b = Unix.rename (tmpdir / a) (tmpdir / b)

let remove f =
  Unix.unlink (tmpdir / f)

let rec poll ~mkdir:m i =
  if m then mkdir tmpdir;
  let events = ref [] in
  let cond = Lwt_condition.create () in

  Irmin_watcher.hook 0 tmpdir (fun e ->
      events := e :: !events;
      Lwt_condition.broadcast cond ();
      Lwt.return_unit
    ) >>= fun unwatch ->
  let reset () = events := [] in
  let rec wait ?n () = match !events with
  | [] -> Lwt_condition.wait cond >>= fun () -> wait ?n ()
  | e  -> match n with
  | None -> reset (); Lwt.return e
  | Some n ->
      if List.length e < n then Lwt_condition.wait cond >>= fun () -> 
        wait ()
      else (reset (); Lwt.return e)
  in
  incr watches_started;

  write "foo" ("foo" ^ string_of_int i) i >>= fun () ->
  incr watches_wrote;
  wait () >>= fun events ->
  unwatch () >>= fun () ->
  decr watches_started;
  return ()

let rec poll_special ~mkdir:m i =
  if m then mkdir tmpdir;
  let events = ref [] in
  let cond = Lwt_condition.create () in

  Irmin_watcher.hook 0 tmpdir (fun e ->
      events := e :: !events;
      Lwt_condition.broadcast cond ();
      Lwt.return_unit
    ) >>= fun unwatch ->
  let reset () = events := [] in
  let rec wait ?n () = match !events with
  | [] -> Lwt_condition.wait cond >>= fun () -> wait ?n ()
  | e  -> match n with
  | None -> reset (); Lwt.return e
  | Some n ->
      if List.length e < n then Lwt_condition.wait cond >>= fun () -> 
        wait ()
      else (reset (); Lwt.return e)
  in

  incr watches_started;
  let will_unwatch_second_last = (!watches_started) = 2 in  

  write "foo" ("foo" ^ string_of_int i) i >>= fun () ->
  incr watches_wrote;

  let did_write_last = (!watches_wrote = !total_watches)  in  
  wait () >>= fun events ->
  unwatch () >>= fun () ->
  decr watches_started;

  if will_unwatch_second_last && did_write_last then
    poll ~mkdir:m i
  else 
  return ()

let random_letter () = Char.(chr @@ code 'a' + Random.int 26)

let rec random_filename () =
  Bytes.init (1 + Random.int 20) (fun _ -> random_letter ())
  |> Bytes.to_string
  |> fun x -> if x = "foo" || x = "bar" then random_filename () else x

let random_path n =
  let rec aux = function
  | 0 -> []
  | n -> random_filename () :: aux (n-1)
  in
  String.concat "/" (aux (n+1))

let random_polls n =
    let rec aux n =  Lwt.join @@ List.init (!total_watches-1) (fun i ->  poll ~mkdir:true i) @ [poll_special ~mkdir:true (!total_watches-1)]
    in
    aux n

let () =
  Irmin_watcher.set_polling_time 0.00001;
  let concurrent_watches = 5 in

  Crowbar.(add_test ~name:"test_watch" [Crowbar.const 1] (fun i -> 
      run (fun () -> random_polls concurrent_watches)
    ))

