type t =
| Say of string option
| Join of Channel.t 
| Leave
| Disconnect 
| Create of Channel.t

exception UnknownCommand
exception WrongParams of string
  
let parse_say msg =
  Say (if msg = "" then None else Some msg)

let parse_command msg =
  let xs = Str.split (Str.regexp " +") msg in 
  match xs with 
  | [] -> raise (Invalid_argument "Message.parse_command")
  | cmd :: params ->
    match cmd with 
    | "/join" -> 
      if List.length params != 1 then raise (WrongParams "/join")  else 
      Join (List.hd params |> Channel.of_string)
    | "/leave" ->
      if List.length params != 0 then raise (WrongParams "/leave") else 
      Leave 
    | "/disconnect" ->
      if List.length params != 0 then raise (WrongParams "/disconnect") else 
      Disconnect
    | "/create" ->
      if List.length params != 1 then raise (WrongParams "/create") else 
      Create (List.hd params |> Channel.of_string)
    | _ -> raise UnknownCommand

let parse msg =
  let msg = String.trim msg in 
  if msg.[0] = '/' then parse_command msg else parse_say msg