type t = string 
let to_string (nick : t) : string = nick 

let of_string (nick : string) : t = 
  let check_valid = fun c -> 
    let code = Char.code c in 
    if not (code >= 97 && code <= 122) then 
      raise (Invalid_argument "Nick.of_string")
  in String.iter check_valid nick; nick
