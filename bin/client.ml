open User
open Utils

let (>>=) = Lwt.bind

let state = State.make ()

let rec handle_user term conn () =
  let open Connection in 
  receive conn.user_input
  >>= fun msg -> 
    send conn msg
  >>= handle_user term conn

let clear term =
  LTerm.clear_screen term 
  >>= fun () ->
    LTerm.goto term {row = 0; col = 0}

let rec handle_server term conn () = 
  let open Connection in 
  receive conn.server_input
  >>= fun msg ->
    let open LTerm_text in let open LTerm_style in
    (match Response.of_string msg with 
    | Response.Fail m ->
      eval [B_fg (rgb 227 57 39); S m; E_fg]
    | Response.Info m ->
      eval [B_fg (rgb 235 226 56); S m; E_fg]
    | Response.Message (nick, m) ->
      eval [B_fg (rgb 69 61 219); S nick; E_fg; S m]
    | Response.Success (code, m) ->
      begin match code with 
      | 1 -> 
        State.change state State.Channel;
        ignore (clear term);
        eval [B_fg (rgb 22 181 56); S m; E_fg]
      | 2 ->
        State.change state State.Hub;
        ignore (clear term);
        eval [B_fg (rgb 22 181 56); S m; E_fg]
      | 3 ->
        eval [B_fg (rgb 22 181 56); S m; E_fg]
      | 4 ->
        State.change state State.Hub;
        ignore (clear term);
        eval [B_fg (rgb 22 181 56); S m; E_fg]
      | _ -> raise Response.UnknownCode
      end)
    |> LTerm.fprintls term
  >>= handle_server term conn 

let handle_connection conn () = 
  Lazy.force LTerm.stdout
  >>= fun term ->
    clear term 
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
