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

let rec eval formula =
	let rec value exp =
		match exp with
		| Num x -> x
		| Plus (x, y) -> value(x) + value(y)
		| Minus (x, y) -> value(x) - value(y)
	in
	match formula with
	| True -> true
	| False -> false
	| Not x -> not (eval(x))
	| AndAlso (x, y) -> eval(x) && eval(y)
	| OrElse (x, y) -> eval(x) || eval(y)
	| Imply (x, y) -> not (eval(x) = true && eval(y) = false)
	| Equal (x, y) -> value(x) = value(y)
