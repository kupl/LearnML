type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval bl = match bl with
		| True -> true
		| False -> false
		| Neg a -> if (eval a) = true then false
			else true
		| Or (a,b) -> if (eval a) = true then true
			else if (eval b) = true then true
			else false
		| And (a,b) -> if (eval a) = false then false
			else if (eval b) = false then false
			else true
		| Imply (a,b) -> if (eval a) = true && (eval b) = false then false
				else true
		| Equiv (a,b) -> if (eval a) = (eval b) then true
				else false
