open User

let rec handle_user conn () =
  let open Connection in 
  receive conn.user_input
  >>= fun msg ->
    send conn msg
  >>= handle_user conn

let rec handle_server conn () = 
  let open Connection in 
  receive conn.server_input
  >>= fun msg ->
    Lwt_io.write Lwt_io.stdout msg 
  >>= handle_server conn 

let handle_connection conn () = 
  Lwt.join [
    handle_user conn ();
    handle_server conn ()
  ]

let create_socket host_name port =
  let open Lwt_unix in 
  let address = 
    try Unix.inet_addr_of_string host_name
    with 
    | Failure _ -> raise (Invalid_argument "Wrong <host-name>") 
  and sock = socket PF_INET SOCK_STREAM 0 in 
  ignore (connect sock (ADDR_INET(address, port)));
  sock 

let _ =
  if Array.length Sys.argv < 3 then 
    raise (Invalid_argument "Usage ./client <host-name> <port-name>");
  let host_name = Sys.argv.(1) and 
  port = int_of_string Sys.argv.(2) in 
  let sock = create_socket host_name port in
  let conn = Connection.make sock in 
  handle_connection conn () |> Lwt_main.run 
