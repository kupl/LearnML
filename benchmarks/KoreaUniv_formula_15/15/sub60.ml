type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> match f with True -> true
 | False -> false
 | Neg p -> if (eval p) = true then true else false
 | Or (p, q) -> if (eval p) = true || eval q = true then true else false
 | And (p, q) -> if eval (p) = true && eval (q) then true else false
 | IMPLY (p, q) -> 
 	if eval (p) = false && eval (q) = false then true 
 	else if eval (p) = true && eval (q) = false then false 
 	else if eval q = true then true 
 	else false
 | Equiv (p, q) -> if (eval (p) = eval (q)) = true then true else false