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

let rec eval k =
	let rec calc e =
		match e with
			| Num(n) -> n
			| Plus(a,b) -> (calc a) + (calc b)
			| Minus(a,b) -> (calc a) - (calc b)
		in
	match k with
		| True -> true
		| False -> false
		| Not(a) -> not (eval a)
		| AndAlso(a,b) -> (eval a) && (eval b)
		| OrElse(a,b) -> (eval a) || (eval b)
		| Imply(a,b) -> (not (eval a)) || (eval b)
		| Equal(a,b) -> (calc a) = (calc b)