open User

let (>>=) = Lwt.bind

let state = State.make ()

let rec handle_user term conn () =
  let open Connection in 
  receive conn.user_input
  >>= fun msg ->
    send conn msg
  >>= handle_user term conn

let rec handle_server term conn () = 
  let open Connection in 
  receive conn.server_input
  >>= fun msg ->
    let open LTerm_text in
    (match state.st with 
     | Title -> eval [B_fg LTerm_style.cyan; S msg; E_fg]
     | Hub -> eval [B_fg LTerm_style.red; S msg; E_fg]
     | Channel -> eval [B_fg LTerm_style.green; S msg; E_fg])
    |> LTerm.fprints term 
  >>= handle_server term conn 

let handle_connection conn () = 
  Lazy.force LTerm.stdout
  >>= fun term ->
    LTerm.clear_screen term 
  >>= fun () ->
    Lwt.choose [
      handle_user term conn ();
      handle_server term conn ();
    ]

let create_socket host_name port =
  let open Lwt_unix in 
  let address = 
    try Unix.inet_addr_of_string host_name
    with 
    | Failure _ -> raise (Failure "Wrong <host-name>") 
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
