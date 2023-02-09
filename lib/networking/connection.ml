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

let send conn response =
  let open Utils.Response in
  (match response with 
   | Success (code, msg) ->
     "Success: " ^ (string_of_int code) ^ " " ^ msg ^ "\n"
   | Fail msg ->
     "Failure: " ^ msg ^ "\n"
   | Message (nick, msg) ->
     (Utils.Nick.to_string nick) ^ ": " ^ msg ^ "\n"
   | Info msg ->
     "Info: " ^ msg ^ "\n")
  |> Lwt_io.write conn.output 
  >>= fun _ ->
    Lwt_io.flush conn.output
  
let receive conn =
  Lwt_io.read_line conn.input >>=
  fun line ->
      Lwt.return line 

let nequal c1 c2 =
    Utils.Id.get_nick c1.id <> Utils.Id.get_nick c2.id

  