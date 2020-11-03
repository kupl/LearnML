type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec eval : formula -> bool
= fun f ->
	match f with
	|True -> true
	|False -> false
	|Not (a) -> not (eval a)
	|AndAlso (a,b) -> eval (a) && eval (b)
	|OrElse (a,b) -> eval (a) || eval (b)
	|Imply (a,b) -> if eval (a) then eval (b) else true
	|Equal (a,b) ->
		let rec _eval x =
			match x with
			|Num (n) -> n
			|Plus (n1, n2) -> _eval (n1) + _eval (n2)
			|Minus (n1, n2) -> _eval (n1) - _eval (n2)
		in
	if _eval (a) < _eval (b) then true else false;;
