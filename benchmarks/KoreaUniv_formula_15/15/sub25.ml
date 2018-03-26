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
				  | True -> true
					| False -> false
					| Neg v -> not (eval v)
					| Or (x,y) -> eval x || eval y
					| And (x,y) -> eval x && eval y
					| Imply (x,y) -> if eval x = true && eval y = false then false
													 else true
					| Equiv (x,y) -> if eval x = eval y then true
													 else false
