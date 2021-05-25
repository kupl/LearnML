type exp = 	Num of int
		| Plus of exp * exp
		| Minus of exp * exp

type formula = 	True
		| False
		| Not of formula
		| AndAlso of formula * formula
		| OrElse of formula * formula
		| Imply of formula * formula
		| Equal of exp * exp

let rec eval (f : formula) : bool = 
	let rec calc (e : exp) : int = match e with
        	| Num x -> x
	        | Plus (x, y) -> (calc x) + (calc y)
		| Minus (x, y) -> (calc x) - (calc y) in
	match f with
		| True -> true
		| False -> false
		| Not x -> not (eval x)
		| AndAlso (x, y) -> (eval x) && (eval y)
		| OrElse (x, y) -> (eval x) || (eval y)
		| Imply (x, y) -> (not (eval x)) || (eval y)
		| Equal (x, y) -> (calc x) = (calc y)

