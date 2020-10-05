open Lwt.Infix

let return = Lwt.return
let max_string_size = 5

let binary_generator = Crowbar.bool
let under_heavy_load () =  Crowbar.sample_from_generator binary_generator
let simulate_load () = if under_heavy_load () then Lwt.pause() else return ()

module Server = struct
    let db = Hashtbl.create 5

    let reset_db () = Hashtbl.clear db

    let find name = 
        let x = Hashtbl.find_opt db name in
        simulate_load () >>= fun () -> return x

    let add name =
        match Hashtbl.find_opt db name with
        | None -> Hashtbl.add db name 1
        | Some x -> Hashtbl.replace db name (x+1)
         
    let check_db () = Hashtbl.fold (fun _ v res -> v = 1 && res) db true

end

let make_request (name : string) : unit Lwt.t = simulate_load ()  >>= fun () ->
    Server.find name >>= fun is_present_in_db -> 
    match is_present_in_db with
    | None -> Server.add name; return ()
    | Some x -> return ()


let () =

    Crowbar.(add_test ~name:"test_multiple_requests" [list1 @@ bytes_fixed max_string_size] (fun l ->
        guard (List.length l < 6);

        Server.reset_db ();

        let waiting_promises = List.map(fun n -> make_request n) l in
        let joined_promise = Lwt.join waiting_promises in
        Lwt_main.run joined_promise;
        check @@ Server.check_db ()
    ))
