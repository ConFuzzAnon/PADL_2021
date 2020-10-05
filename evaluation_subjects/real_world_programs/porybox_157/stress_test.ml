open Lwt.Infix

let return = Lwt.return
let max_string_size = 5

let under_heavy_load () =  Random.bool ()
let simulate_load () = if under_heavy_load () then Lwt.pause () else return ()

module Server = struct
    let db = Hashtbl.create 5

    let reset_db () = Hashtbl.clear db

    let get_box_size name =  
        let x = match Hashtbl.find_opt db name with
        | None -> 0
        | Some y -> y
        in
        simulate_load () >>= fun () -> 
        return x

    let add name size = Hashtbl.replace db name size
        
    let check_db () = Hashtbl.fold (fun _ v res -> res + v) db 0

end

let make_request (name : string) : unit Lwt.t = 
    simulate_load ()  >>= fun () ->
    Server.get_box_size name >>= fun size -> 
    Server.add name (size+1);
    return ()

let () =
    Random.self_init ();
    let max_requests = 5 in

    Crowbar.(add_test ~name:"test_multiple_requests" [list1 @@ bytes_fixed max_string_size] (fun l ->
        guard (List.length l <= max_requests);

        Server.reset_db ();

        let waiting_promises = List.map(fun n -> make_request n) l in
        let joined_promise = Lwt.join waiting_promises in
        Lwt_main.run joined_promise;
        check (Server.check_db () = List.length l)
    ))
