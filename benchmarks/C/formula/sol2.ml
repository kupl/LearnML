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

let rec eval(f:formula) =
	let rec cal(a:exp) =
		match a with
		| Num(k) -> k
		| Plus(b, c) -> cal(b) + cal(c)
		| Minus(b, c) -> cal(b) - cal(c)
	in

	match f with
	|True -> true
	|False -> false
	|Equal(a,b) -> if cal(a)=cal(b) then true else false
	|Not(a) -> not(eval(a))
	|AndAlso(a,b) -> eval(a) && eval(b)
	|OrElse(a,b) -> eval(a) || eval(b)
	|Imply(a,b) -> (not(eval(a))) || eval(b)