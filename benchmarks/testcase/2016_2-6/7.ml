type exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp;;

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
	let rec convert exp =
		match exp with
		| Num(a) -> a
		| Plus(a,b) -> (convert a) + (convert b)
		| Minus(a,b) -> (convert a) - (convert b)
			in
	match f with
	| True -> true
	| False -> false
	| Not(a) -> not (f a)
	| AndAlso(a,b) -> (f a) && (f b)
	| OrElse(a,b) -> (f a) || (f b)
	| Imply(a,b) ->	(f b) || not(f a)
	| Equal(a,b) -> (convert a) = (convert b);;