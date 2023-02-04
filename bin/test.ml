open Lwt

let host = Unix.inet_addr_loopback
let port = 9000
let local_address = Unix.ADDR_INET (Unix.inet_addr_of_string "192.168.1.228", port)
let max_capacity = 10

let counter = ref 0

let rec handle_connection ic oc () =
  Lwt_io.read_line_opt ic >>= fun msg ->
    match msg with 
    | Some msg -> 
      counter := !counter + 1;
      Lwt_io.write_line oc (string_of_int !counter) >>= handle_connection ic oc
    | None -> Logs_lwt.info (fun m -> m "Connection closed") >>= return 

let accept_connection (fd, _) =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd and 
  oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in 
  let handle_err e = Logs.err (fun m -> m "%s" (Printexc.to_string e)) in begin
    Lwt.on_failure (handle_connection ic oc ()) handle_err;
    Logs_lwt.info (fun m -> m "New connection") >>= return
  end

let establish_server socket = 
  let rec serve () =
    Lwt_unix.accept socket >>= accept_connection >>= serve in 
  serve
 
let create_socket () =
  let open Lwt_unix in 
  let socket = socket PF_INET SOCK_STREAM 0 in begin 
    ignore (bind socket local_address);
    listen socket max_capacity;
    socket;
  end 

let _ = 
  Logs.set_reporter (Logs_fmt.reporter ());
  let socket = create_socket () in 
  let serve = establish_server socket in 
  Lwt_main.run (serve ())