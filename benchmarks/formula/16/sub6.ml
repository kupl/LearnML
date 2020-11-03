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
= fun f -> match f with
| True -> true
| False -> false
| Not x -> not (eval x)
| AndAlso (x, y) -> eval x && eval y
| OrElse (x, y) -> eval x || eval y
| Imply (x, y) -> if eval x then eval y else true
| Equal (x, y) -> let rec eval1 g = 
	match g with
	| Num n -> n
	| Plus (g1, g2) -> eval1 g1 + eval1 g2
	| Minus	(g1, g2) -> eval1 g1 - eval1 g2					
in if eval1 x = eval1 y then true else false 	
