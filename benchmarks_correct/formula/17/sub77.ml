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

let rec expval (e : exp) : int =
	match e with
	|Num(i) -> i
	|Plus(i1, i2) -> expval(i1) + expval(i2)
	|Minus(i1, i2) -> expval(i1) - expval(i2)

let rec eval (f : formula) : bool =
	match f with
	|True -> true
	|False -> false
	|Not(f0) -> not(eval(f0))
	|AndAlso(f1, f2) -> eval(f1) && eval(f2)
	|OrElse(f1, f2) -> eval(f1) || eval(f2)
	|Imply(f1, f2) -> not(eval(f1)) || eval(f2)
	|Equal(e1, e2) -> expval(e1) = expval(e2)