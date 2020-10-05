open Lwt.Infix

let x = ref 0
let y = ref 0
let threads = 30

let create_promise_x i = Lwt.pause () >>= fun () -> x := i; Lwt.return_unit
let create_promise_y i = Lwt.pause () >>= fun () -> y := i; Lwt.return_unit

let print_list l = 
        Printf.printf "[";
        List.iter (fun i -> Printf.printf "%d;" i) l;
        Printf.printf "]\n%!"

let () =
Crowbar.(add_test ~name:"test_promise_resolution" [Crowbar.const 1] (fun _ ->

  let l = ref [] in
  let prepend n =
    l := n :: !l
  in 
  Lwt.async(fun () -> ignore @@ List.init threads (fun i -> create_promise_x i); Lwt.return_unit);
  Lwt.async(fun () -> ignore @@ List.init threads (fun i -> create_promise_y i); Lwt.return_unit); 

  let underlying_event, push = Lwt_react.E.create () in
  underlying_event
  |> Lwt_react.E.limit (fun () ->
      let limiter = Lwt_unix.sleep 0.1 in
    Lwt.on_success limiter (fun () ->
        if !x = 1 && !y = 5 then  push 2;
      );  
    limiter)
  |> Lwt_react.E.map prepend 
  |> ignore;


  push 0;
  push 1;
  
  Lwt_main.run (Lwt_unix.sleep 0.15);

  Crowbar.check (!l <> [2;1;0] && !l <> [1;2;0]);

))
