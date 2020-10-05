open Lwt.Infix

let return = Lwt.return
let max_requests = 5

let under_heavy_load () =  Random.bool () 
let simulate_load () = if under_heavy_load () then Lwt_unix.sleep 0.0001 else return ()

module Logger = struct
    let max_file_size = 100
    let current_file_name = ref 1
    let fs = Hashtbl.create 5

    let reset_fs () = Hashtbl.clear fs; Hashtbl.add fs !current_file_name 0

    let replace file_name size = Hashtbl.replace fs file_name ((Hashtbl.find fs file_name)+size)

    let roll_file () = simulate_load () >>= fun () ->
      current_file_name := !current_file_name + 1;
      Hashtbl.add fs !current_file_name 0;
      return ()

    let log size = replace !current_file_name size;
      if Hashtbl.find fs !current_file_name >= max_file_size then roll_file ()
      else return ()

   let check_logger () = Hashtbl.fold (fun _ v res -> if v < max_file_size then res+1 else res) fs 0

end

let make_request wait_for (size : int) : unit Lwt.t = 
  Lwt_unix.sleep (wait_for *. 0.001) >>= fun () ->
  Logger.log size

let () =
  Random.self_init ();
  let max_log_msg_size = 30 in
  let max_wait_duration = 50 in

    Crowbar.(add_test ~name:"test_multiple_requests" [list1 @@ pair (range max_wait_duration) (range max_log_msg_size) ] (fun l ->
        guard (List.length l <= max_requests);

        Logger.reset_fs ();

        let waiting_promises = List.map(fun a -> match a with | t, s -> make_request (Int.to_float t) s) l in
        let joined_promise = Lwt.join waiting_promises in
        Lwt_main.run joined_promise;
        check (Logger.check_logger () < 2)
    ))
