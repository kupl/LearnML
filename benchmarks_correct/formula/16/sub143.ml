type formula =
	|	True
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
let rec va e =
		match e with
		|Num k -> k
		|Plus(k1,k2) -> va k1 + va k2
		|Minus(k1,k2) -> va k1 - va k2
let rec eval : formula -> bool
=  fun f ->
      match f with
      |True -> true
      |False -> false
      |Not (k) -> not(eval k)
      |AndAlso (k1,k2) -> if eval k1 = false  then false else eval k2
      |OrElse (k1,k2) -> if eval k1 = true then true else eval k2
      |Imply (k1,k2) -> not(eval k1) || eval k2
      |Equal (k1,k2) -> va k1 = va k2
