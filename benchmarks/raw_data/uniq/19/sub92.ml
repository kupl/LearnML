(*Cedric Brown*)
let rec uniq lst =
  let rec uniq_lookup a b = 
    match a with
    | [] -> []
    | head :: tail -> if b = head then uniq_lookup tail b 
      else head::(uniq_lookup tail b) in
      
match lst with
  | [] -> []
  | head::tail -> head::(uniq_lookup (uniq tail) head);;
  
(*Example from hw pdf!*)     
uniq [5;6;5;4];;
