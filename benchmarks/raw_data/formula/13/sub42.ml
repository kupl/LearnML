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

let rec eval = fun f ->
	let rec calc = fun ex ->
		match ex with
			Num(a) -> a
			|Plus(a, b) -> (calc a) + (calc b)
			|Minus(a,b) -> (calc a) - (calc b)
	in
	match f with
		True -> true
		| False -> false
		| Not(x) -> not (eval x)
		| AndAlso(x, y) -> (eval x) && (eval y)
		| OrElse(x, y) -> (eval x) || (eval y)
		| Imply(x, y) -> (not (eval x)) || (eval y)
		| Equal(x, y) -> (calc x) = (calc y)
