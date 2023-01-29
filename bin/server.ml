open Utils 
open Networking

let host = Unix.inet_addr_loopback
let port = 9000

let (>>=) = Lwt.bind

let database = Database.make ()

let send_ch chan msg =
  let conns = Database.get_conns database chan in 
  Lwt_list.iter_p (fun conn -> msg ^ "\n" |> Connection.send conn) conns

let on_say conn opt =
  let open Connection in
  match opt with 
  | Some msg ->
    begin try
      let chan = Database.find database conn.id in
      send_ch chan msg
    with Database.NotInChannel -> 
      send conn "You're not connected to any channels \n"
    end
  | None -> Lwt.return_unit

let on_join conn chan =
  let open Connection in
  try
    Database.join_chan database conn.id chan;
    "Succesfully joined: " ^ chan ^ "\n" |> send conn
  with Database.ChannelNotExisting ->
    "Channel " ^ chan ^ " doesn't exist. Use /create [channel_name] \n"
    |> send conn

let on_leave conn =
  let open Connection in
  try
    let chan = Database.find database conn.id in 
    Database.leave_chan database conn.id;
    "You have left " ^ chan ^ "\n" |> send conn 
  with Database.NotInChannel ->
    "You are not connected to any channels \n" |> send conn

let on_disconnect conn =
  let open Connection in
  Database.remove_from_database database conn.id;
  Lwt.return_unit

let on_create conn chan =
  let open Connection in 
  try
    Database.create_channel database chan;
    "Successfully created channel " ^ chan ^ "\n" |> send conn 
  with 
  | Database.NameTaken ->
    "Channel " ^ chan ^ " has already been created. Use /join [channel_name] \n"
    |> send conn
  | Channel.InvalidName msg -> send conn msg

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
    with 
    | Message.UnknownCommand ->
      send conn "Unknown command \n" >>= handle_connection conn
    | Message.WrongParams cmd ->
      (match cmd with 
      | "/join" -> "Usage: /join [channel_name] \n"
      | "/leave" -> "Usage: /leave \n"
      | "/disconnect" -> "Usage: /disconnect \n"
      | "/create" -> "Usage: /create [channel_name] \n"
      | _ -> "") |> send conn 
      >>= handle_connection conn

let rec init conn () =
  let open Connection in
  send conn "Choose your nickname: \n" >>=
    fun _ ->
      receive conn >>=
      fun msg ->
        let id = get_id conn in 
        try 
          Id.set_nick id msg;
          Database.join_server database conn;
          send conn "Success! Welcome to irc-clone chat \n" 
          >>= handle_connection conn
        with
        | Database.NameTaken ->
          ("Nick " ^ msg ^ " is already in use \n" |> send conn)
          >>= init conn  
        | _ -> 
          send conn "Invalid nick, try again \n" 
          >>= init conn 

let rec accept_s socket () =
  Lwt_unix.accept socket >>= 
  fun (fd, sockaddr) ->
    let conn = Connection.make fd sockaddr in 
    init conn () >>= accept_s socket

let establish_server () =
  let open Lwt_unix in
  let addr = ADDR_INET(host, port) in 
  let socket = socket PF_INET SOCK_STREAM 0 in 
  (try
    ignore (bind socket addr)
   with 
   | Unix.Unix_error(Unix.EADDRINUSE, _, _) as e -> 
     raise e);
  listen socket port;
  accept_s socket ()

let _ =
  establish_server () |> Lwt_main.run