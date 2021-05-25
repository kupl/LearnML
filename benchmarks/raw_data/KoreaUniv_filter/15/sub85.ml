(*problem 1*)
let rec filter p l = match l with 
   [] -> []
   | (first :: left) -> 
       if (p first) then first :: (filter p left) else (filter p left)
