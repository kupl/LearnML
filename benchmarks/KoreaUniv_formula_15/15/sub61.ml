type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f ->
match f with
	True -> true
	|False -> false
	|Neg (f1) -> not(eval f1)
	|Or (f1, f2) -> (eval f1) || (eval f2)
	|And (f1, f2) -> (eval f1) && (eval f2)
	|Imply (f1, f2) -> if (eval f1)=true then (eval f2)
	else true
	|Equiv (f1, f2) -> if (eval f1)=(eval f2) then true
	else false;;
