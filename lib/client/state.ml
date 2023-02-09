type state = 
| Title 
| Hub 
| Channel 

exception UnexpectedChange

type t = 
  { lock : Mutex.t;
    mutable st : state }

let make () =
  { lock = Mutex.create ();
    st = Title }

let change x state =
  Mutex.lock x.lock;
  begin match x.st, state with 
  | Title, Hub | Hub, _ | Channel, Hub -> 
    x.st <- state
  | _ -> raise UnexpectedChange
  end;
  Mutex.unlock x.lock;
