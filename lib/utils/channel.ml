type t = string 

exception InvalidName of string
let convention = 
  "Channel names must start with either of following characters: [#,&,+,!],
   can't be longer than 50 characters and can't contain spaces and commas"

let to_string name =
  name 

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


    
  