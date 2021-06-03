type formula = 
True 
| False 
| Not of formula 
| AndAlso of formula * formula 
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec num exp = 
	match exp with
	| Num x -> x
	| Plus (x, y) -> (num x) + (num y)
	| Minus (x, y) -> (num x) - (num y)

let rec eval form =
	match form with 
	| True -> true
	| False -> false
	| Not x -> not (eval x)
	| AndAlso(x, y) -> if( (eval x) = true && (eval y) = true) then true else false
	| OrElse(x, y) -> if( (eval x) = false && (eval y) = false) then false else true
	| Imply(x, y) -> if( (eval x) = true && (eval y) = false ) then false else true
	| Equal(x, y) -> if( (num x) = ( num y)) then true else false
	
