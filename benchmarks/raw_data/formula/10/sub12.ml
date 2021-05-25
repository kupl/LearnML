type exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp
;;

type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
;;

let rec toNumber e =
	match e with Num a -> a
		| Plus (e1, e2) -> (toNumber e1) + (toNumber e2)
		| Minus (e1, e2) -> (toNumber e1) - (toNumber e2)
;;	

let rec eval f =
	match f with True -> true
		| False -> false
		| Not a -> not((eval a))
		| AndAlso (a, b) -> (eval a) && (eval b)
		| OrElse (a, b) -> (eval a) || (eval b)
		| Imply (a, b) -> not (eval a) || (eval b)
		| Equal (a, b) -> ( (toNumber a) = (toNumber b) )
;;
