open Lwt.Infix
open Lwt

let schedule_string = ref ""
let max_run_per_thread = 30

let p, u  = Lwt.task ()

let rec run_p1 n =
  if n <> max_run_per_thread then begin
    Lwt.pause () >>= fun () ->
    schedule_string := !schedule_string ^ "1";
    match !schedule_string with
    | "123123321321132213123123321321132213123123321" ->
        Lwt.wakeup_exn u (Failure "Buggy schedule!"); return ()
    | _ ->
        run_p1 (n+1)
  end
  else Lwt.return ()
     
let rec run_p2 n = 
  if n <> max_run_per_thread then begin
    Lwt.pause () >>= fun () ->
    schedule_string := !schedule_string ^ "2";
    run_p2 @@ n+1
  end
  else Lwt.return ()


let rec run_p3 n = 
  if n <> max_run_per_thread then begin
    Lwt.pause () >>= fun () ->
    schedule_string := !schedule_string ^ "3";
    run_p3 @@ n+1
  end
  else Lwt.return ()

let () = 

  Crowbar.add_test ~name:"schedule check function" [Crowbar.const 1] (fun _ -> 
      schedule_string := ""; 
      let p_4 = Lwt.choose[Lwt.join[run_p1 0; run_p2 0; run_p3 0]; p]in
      Lwt_main.run p_4;
    )
