type formula = True
			| False
			| Not of formula
			| AndAlso of formula * formula
			| OrElse of formula * formula
			| Imply of formula * formula
			| Equal of exp * exp
and exp = Num of int
		| Plus of exp * exp
		| Minus of exp * exp

let rec expEval exp =
	match exp with
		| Num n -> n
		| Plus (a, b) -> (expEval a) + (expEval b)
		| Minus (a, b) -> (expEval a) - (expEval b)

let rec eval formula =
	match formula with
		| True -> true
		| False -> false
		| Not f -> not (eval f)
		| AndAlso (a, b) -> (eval a) && (eval b)
		| OrElse (a, b) -> (eval a) || (eval b)
		| Imply (a, b) -> (not (eval a)) || (eval b)
		| Equal (a, b) -> (expEval a) = (expEval b)
