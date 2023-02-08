let (>>=) = Lwt.bind

type t = 
  { user_input : Lwt_io.input_channel;
    server_input : Lwt_io.input_channel;
    output : Lwt_io.output_channel }

let make fd =
  { user_input = Lwt_io.stdin;
    server_input = Lwt_io.of_fd ~mode:Lwt_io.input fd;
    output = Lwt_io.of_fd ~mode:Lwt_io.output fd }

let send conn msg =
  Lwt_io.write conn.output msg >>= fun _ ->
    Lwt_io.flush conn.output
      
let receive input_ch =
  Lwt_io.read_line input_ch
  >>= fun line ->
      Lwt.return (line ^ "\n") 