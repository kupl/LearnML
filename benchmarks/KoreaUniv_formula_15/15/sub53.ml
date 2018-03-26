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
	| Neg b -> eval b = false
	| Or (b1, b2) -> eval b1 || eval b2
	| And (b1, b2) -> eval b1 && eval b2
	| Imply (b1, b2) -> eval (Neg b1) || eval b2
	| Equiv (b1, b2) -> eval b1 = eval b2;;
