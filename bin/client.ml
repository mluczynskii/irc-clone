open Notty
open Notty_lwt 
let port = 9001

let (>>=) = Lwt.bind

let rec loop t =
  let img = I.(string A.(bg lightred ++ fg black) "Sample text") in
  ignore (Term.image t img);
  loop t
let interface () =
  let tc = Unix.(tcgetattr stdin) in
  Unix.(tcsetattr stdin TCSANOW { tc with c_isig = false });
  let terminal = Term.create () in
  loop terminal

let handle_connection () = Lwt.return_unit
  
let start () =
  Lwt.choose [
    interface ();
    handle_connection ()
  ]

let _ =
  let open Unix in
  if Array.length Sys.argv < 2 then 
    raise (Invalid_argument "Usage ./client <host-name>");
  let host_name = Sys.argv.(1) in 
  let address =
    try 
      (gethostbyname host_name).h_addr_list.(0) 
    with 
    | Not_found -> Invalid_argument (host_name ^ ": Host not found") |> raise 
  in 
  let socket = socket PF_INET SOCK_STREAM 0 in 
  connect socket (ADDR_INET(address, port));
  Lwt_main.run (start ())
