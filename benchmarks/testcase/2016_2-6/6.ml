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

(*let rec exp : int -> int
= fun n ->
	match n with
	| Num k -> k
	| Plus (k, l) -> exp(k) + exp(l)
	| Minus (k, l) -> exp(k) - exp(l);;*)

let rec f : formula -> bool
= fun form -> 
	match f with 
	| True -> true
	| False -> false
	| Not p -> not ( f (p) )
	| AndAlso (p, q) -> f (p) && f (q)
	| OrElse (p, q) -> f (p) || f (q)
	| Imply (p, q) -> not ( f (p) ) || f(q)
	| Equal (p, q) -> p = q ;;
		(*if exp(p) = exp(q) then true
		else false;;*)




