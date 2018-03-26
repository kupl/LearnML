type formula = True | False | Neg of formula | Or of formula * formula | And of formula * formula | Imply of formula * formula | Equiv of formula * formula
let rec eval p =
	match p with
	True -> true
	| False -> false
	| Neg p -> not (eval p)
	| Or (a, b) -> (eval a) || (eval b)
	| And (a, b) -> (eval a) && (eval b)
	| Imply (a, b) -> (eval a) && (eval (Neg b))
	| Equiv (a, b) -> (eval (Neg a)) && (eval b)
