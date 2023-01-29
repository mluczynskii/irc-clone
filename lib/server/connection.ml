let (>>=) = Lwt.bind

type t = 
  { input : Lwt_io.input_channel;
    output : Lwt_io.output_channel;
    mutable id : Utils.Id.t }

let make fd sockaddr =
  let address = (Unix.getnameinfo sockaddr []).Unix.ni_hostname in
  { input = Lwt_io.of_fd ~mode:Lwt_io.input fd;
    output = Lwt_io.of_fd ~mode:Lwt_io.output fd;
    id = Utils.Id.make_opt None (Some address) }

let set_id conn id =
  if Utils.Id.is_valid id then conn.id <- id

let get_id conn = conn.id

let send conn msg =
  Lwt_io.write conn.output msg
 
let rec receive conn =
  Lwt_io.read_line conn.input >>=
  fun line ->
    try 
      Lwt.return line 
    with _ -> raise End_of_file

let nequal c1 c2 =
    Utils.Id.get_nick c1.id <> Utils.Id.get_nick c2.id

  