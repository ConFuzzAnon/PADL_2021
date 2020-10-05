open Lwt.Infix

let return = Lwt.return
let ip = "sample"
let communication_channel = Lwt_mvar.create_empty ()

type entry =
  | Incomplete of unit Lwt_condition.t
  | Verified

let arp_cache = Hashtbl.create 1

let process_arp_request t = Lwt_unix.sleep t

let reply_arp_query sleep_time =
    Lwt_mvar.take communication_channel >>= fun ip ->
    Lwt_unix.sleep sleep_time >>= fun () ->
    Lwt.pause () >>= fun () ->
    match Hashtbl.find_opt arp_cache ip with
    | Some (Incomplete cond) -> Hashtbl.replace arp_cache ip (Verified); Lwt_condition.broadcast cond (); return ()
    | _ -> return ()

let send_query_arp_packet ip sleep_time = 
    Lwt.pause () >>= fun () ->
    Lwt_mvar.put communication_channel ip >>= fun () ->
    Lwt_unix.sleep sleep_time >>= fun () ->
    Lwt.pause () 

let query ip sleep_time = let cond = Lwt_condition.create () in
    Hashtbl.add arp_cache ip (Incomplete cond);
    send_query_arp_packet ip sleep_time >>= fun () ->
    Lwt_condition.wait cond

let () = let max = 50 in
Crowbar.(add_test ~name:"test_arp_query" [Crowbar.range ~min:1 max; Crowbar.range ~min:max max] (fun _i _j->
    Hashtbl.clear arp_cache;
    ignore @@ Lwt_mvar.take_available communication_channel;

    let p = reply_arp_query (0.001 *. Float.of_int _j) in
    let query_promise = query ip (0.001 *. Float.of_int _i)in
    Lwt_main.run p;

    let assert_cache_entry ip = 
        match Hashtbl.find_opt arp_cache ip with
                                 | Some (Verified) -> false
                                 | _ -> true
    in

    Crowbar.check (not @@ Lwt.is_sleeping query_promise || assert_cache_entry ip);

))
