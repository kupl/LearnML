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
	let rec ma2 e =
		match e with
		| Num (x) -> x 
		| Plus (x, y) -> (ma2 x) + (ma2 y)
		| Minus (x, y) -> (ma2 x) - (ma2 y)		
	in let rec ma p = 
		match p with 
		| True -> true
		| False -> false
		| Not(x) -> not(ma x)
		| AndAlso (x,y) -> (ma x) && (ma y)
		| OrElse (x,y) -> (ma x) || (ma y)
		| Imply (x,y) -> if ma x = true && ma y = false then false else true
		| Equal (x,y) -> if (ma2 x) = (ma2 y) then true else false
	in ma f