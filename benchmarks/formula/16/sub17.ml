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
	let rec operate exp = 
		match exp with 
			|Num(a) -> a 
			|Plus(a, b) -> operate(a) + operate(b) 
			|Minus(a, b)-> operate(a) - operate(b) in
    match f with
    |True   -> true
    |False  -> false
    |Not(a) -> not (eval(a))
    |AndAlso (a,b) -> eval(a) && eval(b)
    |OrElse  (a,b) -> eval(a) || eval(b)
    |Imply   (a,b) -> not (eval(a)) || eval(b)
    |Equal   (a,b) -> operate(a)=operate(b);;