type t =
| Success of int * string
| Message of Nick.t * string 
| Fail of string
| Info of string

exception UnknownCode

let join msg = Success (1, msg)
let leave msg = Success (2, msg)
let create msg = Success (3, msg)
let welcome msg = Success (4, msg)

let of_string str =
  let msg = String.trim str in
  let xs = Str.split (Str.regexp " +") msg in
  match xs with 
  | h :: t ->
    begin match h with 
    | "Info:" -> Info (String.concat " " t)
    | "Failure:" -> Fail (String.concat " " t)
    | "Success:" ->
      begin match t with 
      | code :: msg -> Success (int_of_string code, String.concat " " msg)
      | [] -> raise (Invalid_argument "Response.of_string") 
      end
    | _ -> Message (h, String.concat " " t)
    end
  | [] -> raise (Invalid_argument "Response.of_string")