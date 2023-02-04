type t =
  { mutable nick : Nick.t option;
    mutable address : string option}

exception InvalidId

let make_opt nick address = { nick; address }
let make nick address = 
  { nick = if nick = "" then None else Some (Nick.of_string nick);
    address = if address = "" then None else Some address }

let is_valid id =
  id.nick <> None && id.address <> None

let get_nick id = 
  match id.nick with 
  | None      -> raise InvalidId
  | Some nick -> nick

let set_nick id nick =
  if nick = "" then 
    raise InvalidId
  else  
    id.nick <- Some (Nick.of_string nick)
