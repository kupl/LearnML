type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> match f with
	True -> true
	| False -> false
	| Neg (a) -> if a = True then false else true   
	| Or (a, b) -> if(a = True || b = True) then true else false
	| And (a, b) -> if(a = True && b = True) then true else false
	| Imply (a, b) -> if a = True then true else false
	| Equiv (a, b) -> if((a = True && b = True) || (a = False && b = False)) then true else false
