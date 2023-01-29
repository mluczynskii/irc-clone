open Utils

type t =
  { id_to_chan : (Id.t, Channel.t) Hashtbl.t;
    nick_to_conn : (Nick.t, Connection.t) Hashtbl.t;
    chan_to_conns : (Channel.t, Connection.t list) Hashtbl.t }

exception NotInChannel
exception NameTaken
exception ChannelNotExisting

let (<>) = Connection.nequal 

let make () = 
  { id_to_chan = Hashtbl.create 4;
    nick_to_conn = Hashtbl.create 4;
    chan_to_conns = Hashtbl.create 4 }

let get_conns db chan = 
  match Hashtbl.find_opt db.chan_to_conns chan with 
  | None    -> []
  | Some xs -> xs

let check_taken db nick =
  Hashtbl.mem db.nick_to_conn nick

let find db id = 
  match Hashtbl.find_opt db.id_to_chan id with 
  | None      -> raise NotInChannel
  | Some chan -> chan

let join_server db conn =
  let id = Connection.get_id conn in 
  let nick = Id.get_nick id in
  if Hashtbl.mem db.nick_to_conn nick then raise NameTaken else 
    Hashtbl.add db.nick_to_conn nick conn

let leave_chan db id =
  match Hashtbl.find_opt db.id_to_chan id with 
  | None      -> raise NotInChannel
  | Some chan ->
    let nick = Id.get_nick id in
    let conn = Hashtbl.find db.nick_to_conn nick in
    Hashtbl.remove db.id_to_chan id;
    Hashtbl.replace db.chan_to_conns chan (List.filter (fun c -> c <> conn) (get_conns db chan))

let join_chan db id chan =
  if not (Hashtbl.mem db.chan_to_conns chan) then raise ChannelNotExisting;
  if Hashtbl.mem db.id_to_chan id then leave_chan db id;
  let conn = Hashtbl.find db.nick_to_conn (Id.get_nick id) and
  xs = get_conns db chan in 
  Hashtbl.replace db.chan_to_conns chan (conn :: xs);
  Hashtbl.add db.id_to_chan id chan

let remove_from_database db id =
  Hashtbl.remove db.nick_to_conn (Id.get_nick id);
  leave_chan db id

let create_channel db chan =
  if Hashtbl.mem db.chan_to_conns chan then raise NameTaken else 
  Hashtbl.add db.chan_to_conns chan []  
    


