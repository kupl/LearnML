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

let rec evale : exp -> int = fun exp ->
	match exp with
	| Num n -> n
	| Plus (exp1, exp2) -> evale(exp1) + evale(exp2)
	| Minus (exp1, exp2) -> evale(exp1) - evale(exp2)

let rec eval : formula -> bool = fun f ->
	match f with
	| True -> true
	| False -> false
	| Not f1 -> not(eval(f1))
	| AndAlso (f1, f2) -> (if eval(f1) then eval(f2) else false)
	| OrElse (f1, f2) -> (if eval(f1) then true else eval(f2))
	| Imply (f1, f2) -> (if (eval(f1) && (not (eval(f2)))) then false else true)
	| Equal (exp1, exp2) -> (evale(exp1) = evale(exp2))
