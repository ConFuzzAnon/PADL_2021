open Lwt.Infix

let return = Lwt.return
let max_string_size = 4
let max_path_length = 4
let max_request = 4

let binary_generator = Crowbar.bool
let under_heavy_load () =  Crowbar.sample_from_generator binary_generator
let simulate_load () = if under_heavy_load () then Lwt_unix.sleep 0.05 else return ()

let split path = path |> String.split_on_char Core.Filename.dir_sep.[0]
    |> List.filter (fun s -> s != "")

let rec sublist b e l = 
  match l with
    [] -> failwith "sublist"
  | h :: t -> 
     let tail = if e=0 then [] else sublist (b-1) (e-1) t in
     if b>0 then tail else h :: tail

exception MkdirP_EEXIST of string * string

let rec mkdirP path =
    if Core.Filename.is_relative path then Lwt.fail @@ Failure "Relative path"
    else (
        let _  = split path in
        Lwt_unix.file_exists path >>= fun exists ->
        if exists then return ()
        else (

          let mkdir_syscall () = Lwt_unix.mkdir path 0o755 in

          Lwt.try_bind (fun () -> mkdirP @@ Core.Filename.dirname path) (mkdir_syscall) (fun e ->
              match e with
              | MkdirP_EEXIST _ -> mkdir_syscall () 
              | _ -> raise e
            )
        )
    )

let rec dir_exists l = match l with
    | [] -> true
    | h::t -> 
        if Sys.file_exists h then dir_exists t
        else false

let file_paths_gen () = 
    let open Crowbar in
   let dir_gen :string gen = map [int16] (fun n -> Printf.sprintf "%X" n) in
    let path_gen: string gen = map [list1 dir_gen] (fun l -> guard(List.length l <= max_path_length); String.concat Core.Filename.dir_sep l) in
    let file_paths_gen : string list gen = map [list1 path_gen] (fun l -> guard(List.length l <= max_request); l) in
    file_paths_gen

let check_and_get_test_dir () =  
    let root_dir = Core.Filename.realpath "." ^ "/testdir/"  in
    if not @@ Sys.file_exists root_dir then (
        Unix.mkdir root_dir 0o755;
        root_dir 
    ) else root_dir

let () =
    let open Crowbar in
    let root_dir = check_and_get_test_dir () in
     
    Crowbar.(add_test ~name:"test_multiple_mkdir_requests" [file_paths_gen ()] (fun l ->

        let _ = Sys.command ("rm -rf " ^ root_dir ^ "/*") in
        let root_list = List.map (fun p -> root_dir ^ p) l in
        let waiting_promises = List.map (fun p -> simulate_load () >>= fun () -> mkdirP p) root_list in
        let joined_promise = Lwt.join waiting_promises in
        let are_all_dir_created () = check @@ dir_exists root_list in

        let run_main () = try Lwt_main.run joined_promise with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
        in

        run_main ();
        are_all_dir_created ()
   ))
