type exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

let rec f : formula -> bool
= fun form ->
	match form with
	|True -> true
	|False -> false
	|Not (a) -> not (f a)
	|AndAlso (a,b) -> f (a) && f (b)
	|OrElse (a,b) -> f (a) || f (b)
	|Imply (a,b) -> if f (a) then f (b) else true
	|Equal (a,b) ->
		let rec _eval x =
			match x with
			|Num (n) -> n
			|Plus (n1, n2) -> _eval (n1) + _eval (n2)
			|Minus (n1, n2) -> _eval (n1) - _eval (n2)
		in
	if _eval (a) < _eval (b) then true else false;;
