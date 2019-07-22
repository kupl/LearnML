(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
    [] -> []
  | head :: [] -> if pred head = true then lst
                  else []
  | head :: tail -> 
    if pred head = true then head::(filter pred tail)
    else filter pred tail
