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
	| True -> true
	| False -> false
	| Not (f1) -> not(f(f1))
	| AndAlso (f1,f2) -> f(f1) && f(f2)
	| OrElse (f1,f2) -> f(f1) || f(f2)
	| Imply (f1,f2) -> f(Not (f1)) && f(f2)
	| Equal (e1,e2) ->((cal e1) = (cal e2))
;;