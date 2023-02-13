open Utils 
open Networking
let max_capacity = 16

let (>>=) = Lwt.bind

let database = Database.make ()

let send_ch id chan msg =
  let nick = Id.get_nick id in
  let conns = Database.get_conns database chan in 
  let response = Response.Message (nick, msg) in
  Lwt_list.iter_p (fun conn -> Connection.send conn response) conns

let on_say conn opt =
  let open Connection in
  match opt with 
  | Some msg ->
    begin try
      let chan = Database.find database conn.id in
      send_ch conn.id chan msg
    with Database.NotInChannel -> 
      let response = Response.Fail "You're not connected to any channels" in
      send conn response
    end
  | None -> Lwt.return_unit 

let on_join conn chan =
  let open Connection in
  try
    Database.join_chan database conn.id chan;
    let response = "Joined " ^ chan |> Response.join in
    send conn response
  with 
  | Database.ChannelNotExisting ->
    let response = Response.Fail ("Channel " ^ chan ^ " doesn't exist. Use /create [channel_name])") in
    send conn response
  | Channel.InvalidName msg ->
    Response.Fail msg |> send conn 

let on_leave conn =
  let open Connection in
  try
    let chan = Database.find database conn.id in 
    Database.leave_chan database conn.id;
    Response.leave ("You have left the channel" ^ chan) |> send conn 
  with Database.NotInChannel ->
    Response.Fail "You are not connected to any channels" |> send conn

let on_disconnect conn =
  let open Connection in
  Response.disconnect ("You have disconnected from the server") |> send conn
  >>= fun () ->
    Database.remove_from_database database conn.id;
    Lwt.return_unit

let on_list conn = 
  let open Connection in 
  let conns = Database.get_channel_names database in 
  let str = String.concat " " conns in 
  Response.Info str |> send conn 

let on_create conn chan =
  let open Connection in 
  try
    Database.create_channel database chan;
    Response.create ("Created channel " ^ chan) |> send conn 
  with 
  | Database.NameTaken ->
    Response.Fail ("Channel " ^ chan ^ " has already been created. Use /join [channel_name]")
    |> send conn
  | Channel.InvalidName msg -> Response.Fail msg |> send conn 

let rec handle_connection conn () =
  let open Connection in
  receive conn >>=
  fun msg ->
    try
      let msg = Message.parse msg in
      match msg with 
      | Say opt -> on_say conn opt >>= handle_connection conn
      | Join chan -> on_join conn chan >>= handle_connection conn
      | Leave -> on_leave conn >>= handle_connection conn
      | Disconnect -> on_disconnect conn
      | Create chan -> on_create conn chan >>= handle_connection conn
      | List -> on_list conn >>= handle_connection conn
    with 
    | Message.UnknownCommand ->
      send conn (Response.Fail "Unknown command") >>= handle_connection conn
    | Message.WrongParams cmd ->
      Response.Fail
      (match cmd with 
      | "/join" -> "Usage: /join [channel_name]"
      | "/leave" -> "Usage: /leave"
      | "/disconnect" -> "Usage: /disconnect"
      | "/create" -> "Usage: /create [channel_name]"
      | "/list" -> "Usage: /list"
      | _ -> "gluurb") 
      |> send conn 
      >>= handle_connection conn

let rec init conn () =
  let open Connection in
  Response.Info "Choose your nickname: "
  |> send conn  
  >>= fun _ ->
    receive conn 
  >>= fun msg ->
    let id = get_id conn in 
    try 
      Id.set_nick id msg;
      Database.join_server database conn;
      Response.welcome "Welcome to irc-clone chat"
      |> send conn
      >>= handle_connection conn
    with
    | Database.NameTaken ->
      Response.Fail ("Nick " ^ msg ^ " is already in use") 
      |> send conn
      >>= init conn  
    | _ -> 
      Response.Fail "Invalid nick, try again" 
      |> send conn 
      >>= init conn 

let create_socket () =
  let open Lwt_unix in 
  let host = Sys.argv.(1) and port = int_of_string Sys.argv.(2) in
  let addr = ADDR_INET (Unix.inet_addr_of_string host, port) in
  let socket = socket PF_INET SOCK_STREAM 0 in begin 
    (try 
      ignore (bind socket addr)
    with 
    | Unix.Unix_error(Unix.EADDRINUSE, _, _) as e -> 
      raise e);
    listen socket max_capacity;
    socket 
  end

let establish_server socket = 
  let rec serve () =
    Lwt_unix.accept socket 
    >>= fun (fd, sockaddr) ->
      let conn = Connection.make fd sockaddr in 
      let handle_err e = Database.remove_from_database database conn.id in
      Lwt.on_failure (init conn ()) handle_err |> Lwt.return
    >>= serve 
  in serve

let _ =
  if Array.length Sys.argv < 3 then 
    raise (Invalid_argument "Usage: ./server.exe <host-name> <port-name>");
  let socket = create_socket () in 
  let serve = establish_server socket in
  serve () |> Lwt_main.run