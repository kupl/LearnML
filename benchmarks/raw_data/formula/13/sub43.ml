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

let rec numExpr ex = 
	match ex with 
	| Num n -> n
	| Plus (n1,n2) -> ((numExpr n1) + (numExpr n2))
	| Minus (n1,n2) -> ((numExpr n1) - (numExpr n2))

let rec eval value = 
	match value with
	| True -> true
	| False -> false
	| Not f -> not (eval f)
	| AndAlso (f1,f2) ->  (eval f1) && (eval f2)
	| OrElse (f1,f2) ->  (eval f1) || (eval f2)
	| Imply (f1,f2) -> (if (eval f1) = true && (eval f2) = false then false
							else true)
	| Equal (e1,e2) -> (if (numExpr e1) = (numExpr e2) then true
						else false)
