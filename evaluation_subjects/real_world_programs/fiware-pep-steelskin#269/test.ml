open Lwt.Infix

let return = Lwt.return
let max_requests = 2

let binary_generator = Crowbar.bool
let if_under_load () =  Crowbar.sample_from_generator binary_generator
let simulate_load () = if if_under_load () then Lwt_unix.sleep 0.0001 else return ()

module KeyStoneAuth = struct

    let currentToken = ref None
    let authenticating = ref false
    let waiting_requests = ref None

    let reset_authenticator () = currentToken := None; authenticating := false; waiting_requests := None

    let get_resolver () = match !waiting_requests with
        | None -> failwith "get_resolver waiting_requests is None"
        | Some ( _ , y) -> y

    let get_promise () = match !waiting_requests with
        | None -> failwith "get_promise waiting_requests is None"
        | Some (x , _) -> x

    let auth_request_to_key_stone () = Lwt.ignore_result (simulate_load () >>= fun () ->
        authenticating := false;
        currentToken := Some 1;
        if Lwt.is_sleeping (get_promise ()) then (
            Lwt.wakeup_later (get_resolver ()) () 
        );
        return () )

    let authenticate () = if Option.is_none !currentToken then (
            authenticating := true;
            waiting_requests := Some (Lwt.wait ());
            let p = get_promise () in
            auth_request_to_key_stone (); 
            p
         )else if !authenticating then (
            get_promise ())
        else (
            return ())

    let did_handle_all_requests () = not !authenticating && Option.is_some !currentToken && not @@ Lwt.is_sleeping (get_promise ())

    end


let make_request t = 
    Lwt_unix.sleep (t *. 0.001)  >>= fun () ->
    KeyStoneAuth.authenticate () >>= fun () ->
    return ()

let did_request_hang waiting_promises = List.fold_left (fun b p -> b || Lwt.is_sleeping p) false waiting_promises 

let () =
  let max = 50 in
  Crowbar.(add_test ~name:"test_multiple_requests" [range max; range ~min:max max] (fun t1 t2 ->
        KeyStoneAuth.reset_authenticator ();

        let waiting_promises = [make_request @@ Int.to_float t1; make_request @@ Int.to_float t2] in
        let joined_promise = Lwt.join waiting_promises in
        Lwt_main.run @@ Lwt.pick [joined_promise; Lwt_unix.sleep 0.200];
        let completed_all_requests = not (KeyStoneAuth.did_handle_all_requests () && did_request_hang waiting_promises) in
        check @@ completed_all_requests
        
    ))
