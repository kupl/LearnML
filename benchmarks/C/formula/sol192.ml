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
let rec nexp(e) =
	match e with
	| Num n -> n
	| Plus(e1,e2) -> (nexp(e1))+(nexp(e2))
	| Minus(e1,e2) -> (nexp(e1))-(nexp(e2))
let rec eval(f) =
	match f with
	| True -> true
	| False -> false
	| Not ff -> not (eval(ff))
	| AndAlso (f1,f2) -> (eval(f1)) && (eval(f2))
	| OrElse (f1,f2) -> (eval(f1)) || (eval(f2))
	| Imply (f1,f2) -> ((not(eval(f1))) || (eval(f2)))
	| Equal (e1,e2) -> (nexp(e1)) = (nexp(e2))
