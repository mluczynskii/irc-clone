type t = string 

exception InvalidName of string
let convention = 
  "Channel names must start with either of following characters: [#,&,+,!] 
   and can't be longer than 50 characters"

let to_string name =
  name 

(* RFC 2812 1.3 Channels*)
let of_string name =
  let check_valid name =
    let length = String.length name and pref = name.[0] in 
    length > 0 && length <= 50
    && (pref = '#' || pref = '&' || pref = '+' || pref = '!')
    && not (String.contains name ' ')
    && not (String.contains name ',')
    && not (Char.chr 7 |> String.contains name) in 
  if check_valid name then 
    String.lowercase_ascii name 
  else 
    raise (InvalidName convention)


    
  