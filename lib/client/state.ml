type state = 
| Title 
| Hub 
| Channel 

type t = state ref

let change x state =
  match !x, state with 
  | Title, Hub -> x := Hub 
  | Hub, _ -> x := state 
  | Channel, Hub -> x := Hub 
  | _ -> raise (Invalid_argument "State.change")
