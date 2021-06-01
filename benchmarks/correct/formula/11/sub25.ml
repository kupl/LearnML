type formula = True | False | Not of formula | AndAlso of formula * formula | OrElse of formula * formula | Imply of formula * formula | Equal of exp * exp
and exp = Num of int | Plus of exp * exp | Minus of exp * exp



let rec eval f =
	let rec calc _exp =
		match _exp with 
		Num(_num) -> _num
		| Plus(_exp1, _exp2) -> (calc _exp1) + (calc _exp2)
		| Minus(_exp1, _exp2) -> (calc _exp1) - (calc _exp2)
	in

	match f with
	True -> true
	| False -> false
	| Not x -> not (eval x)
	| AndAlso(a, b) -> (eval a) && (eval b)
	| OrElse(a, b) -> (eval a) || (eval b)
	| Imply(a, b) -> (not (eval a)) || (eval b)
	| Equal(a, b) -> (calc a) = (calc b)
