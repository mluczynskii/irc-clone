type t =
  { mutable nick : Nick.t option;
    mutable address : string option}

exception InvalidId

let make_opt nick address = { nick; address }
let make nick address = 
  let open Nick in
  { nick = if nick = "" then None else Some (of_string nick);
    address = if address = "" then None else Some address }

let is_valid id =
  id.nick <> None && id.address <> None

let get_nick id = 
  match id.nick with 
  | None      -> raise InvalidId
  | Some nick -> nick

let set_nick id nick =
  let open Nick in
  if nick = "" then 
    raise InvalidId
  else  
    id.nick <- Some (of_string nick)
let set_address id addr =
  if addr = "" then 
    raise InvalidId
  else  
    id.address <- Some addr

