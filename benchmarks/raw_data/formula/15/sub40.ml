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


let rec eeval f =
	match f with
	|	Num a -> a
	| Plus (a, b) -> (eeval a) + (eeval b)
	| Minus (a, b) -> (eeval a) - (eeval b)


let rec eval f =
	match f with
	| True -> true
	| False -> false
	| Not a -> not (eval a) 
	| AndAlso (a, b) -> (eval a) && (eval b) 
	| OrElse (a, b) -> (eval a) || (eval b)
	| Imply (a, b) -> if (eval a)=false then true
										else (eval b)
	| Equal (a, b) -> (eeval a) = (eeval b)