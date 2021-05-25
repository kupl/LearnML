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
;;

let rec eval form =
	match form with
	| True -> true
	| False -> false
	| Not first -> if eval first then false else true
	| AndAlso (first, second) -> (eval first) && (eval second)
	| OrElse (first, second) -> (eval first) || (eval second)
	| Imply (first, second) -> eval (OrElse (Not first, second))
	| Equal (first, second) -> let rec eval2 exp =
		match exp with
		| Num val1 -> val1
		| Plus (val1, val2) -> (eval2 val1) + (eval2 val2)
		| Minus (val1, val2) -> (eval2 val1) - (eval2 val2)
		in
		(eval2 first) = (eval2 second)
;;


