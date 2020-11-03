
type formula = True
			| False
			| Not of formula
			| AndAlso of formula * formula
			| OrElse of formula * formula
			| Imply of formula * formula
			| Equal of exp * exp
	and exp 	= Num of int
				| Plus of exp * exp
				| Minus of exp * exp

;;
let rec expeval exp =
	match exp with
		Num n -> n
		| Plus (n1,n2) -> (expeval n1) + (expeval n2)
		| Minus (n1,n2) -> (expeval n1) - (expeval n2)
;;
let rec eval form =
	match form with
		True -> true
		| False -> false
		| Not f -> if (eval f) then false else true
		| AndAlso (f1,f2) -> ((eval f1) && (eval f2))
		| OrElse (f1,f2) -> ((eval f1) || (eval f2))
		| Imply (f1,f2) -> if (eval f1) && (eval f2) = false then false else true
		| Equal (e1,e2) -> ((expeval e1) = (expeval e2))
;;

