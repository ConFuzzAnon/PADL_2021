open Lwt.Infix

let return = Lwt.return

module Bank = struct
    let bank_total = ref 0
    let last_written_bal = ref 0
  
    let process_tx sum = bank_total := !bank_total + sum;
      Lwt_unix.sleep 0.05 >>= fun () ->
      if !last_written_bal = !bank_total then
        return ()
      else (
        let x = !bank_total in 
        Lwt_io.with_file ~mode:Lwt_io.output ~flags: [Unix.O_WRONLY;Unix.O_APPEND; Unix.O_CREAT; Unix.O_NONBLOCK] "bank.log" (fun oc -> Lwt_io.write_line oc @@ Int.to_string x) >>=
        fun  () -> last_written_bal := !bank_total; return ()
      )

    let reset () = bank_total := 0;
      Lwt_main.run (Lwt_io.with_file ~mode:Lwt_io.output "bank.log" (fun _ -> return ()) )

    let check_log_consitency () = 
      let total = ref 0 in
      Lwt_io.open_file ~mode:Lwt_io.input "bank.log" >>=
      fun ic -> 
        let rec read_all () = 
          Lwt.try_bind (fun () ->  Lwt_io.read_line ic)
            (fun s -> total := !total + int_of_string s ; read_all ())
            (fun exn -> return ())
        in
        read_all () >>=
        fun () -> Lwt_io.close ic >>= fun () -> return !total

end


let () =
    let size = 50 in
    let sleep_timers = Array.init size (fun i -> 0.001 *. Float.of_int (i + 1)) in
    Crowbar.(add_test ~name:"test_bank_tx_atomicity" [Crowbar.range size] (fun i ->
        Bank.reset ();

        let sum_1 = 100 and sum_2 = 200 in
        let trx_1 () = Bank.process_tx sum_1 in
        let trx_2 i = Lwt_unix.sleep @@ i +. 0.05  >>= fun () ->  Bank.process_tx sum_2 in
        let final_promise = Lwt.join[ trx_1 (); trx_2 @@ sleep_timers.(i);] >>= fun () -> Bank.check_log_consitency () in
        let final_sum = Lwt_main.run final_promise in
        Crowbar.check ((sum_1  + sum_1 + sum_2) = final_sum)
    ))
