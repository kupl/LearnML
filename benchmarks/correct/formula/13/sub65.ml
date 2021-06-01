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
let rec valueofexp exp =
	match exp with
	| Num (num) -> num
	| Plus (e1, e2) -> valueofexp(e1) + valueofexp(e2)
	| Minus (e1, e2) -> valueofexp(e1) - valueofexp(e2)
let rec eval form = 
	match form with
	| True -> true
	| False -> false
	| Not (formula) -> not(eval(formula))
	| AndAlso (f1, f2) -> eval(f1) && eval(f2)
	| OrElse (f1, f2) -> eval(f1) || eval(f2)
	| Imply (f1, f2) -> not(eval(f1)) || eval(f2)
	| Equal (e1, e2) -> if valueofexp(e1) = valueofexp(e2) then true else false
