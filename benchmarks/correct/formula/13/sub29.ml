type exp = Num of int
  	| Plus of exp * exp
  	| Minus of exp * exp
  
type formula = True | False
  	| Not of formula
  	| AndAlso of formula * formula
  	| OrElse of formula * formula
  	| Imply of formula * formula
  	| Equal of exp * exp

let rec etoi exp = 
  	match exp with
  	| Num a -> a
  	| Plus (exp1, exp2) -> etoi exp1 + etoi exp2
  	| Minus (exp1, exp2) -> etoi exp1 - etoi exp2

let rec eval f =
  	match f with
  	| True -> true
  	| False -> false
  	| Not (p) -> not (eval p)
	| AndAlso (p, q) -> (eval p) && (eval q)
	| OrElse (p, q) -> (eval p) || (eval q)
	| Imply (p, q) -> not (eval p) || (eval q)
  	| Equal (exp1, exp2) ->
  		if (etoi exp1) = (etoi exp2) then true
		else false
