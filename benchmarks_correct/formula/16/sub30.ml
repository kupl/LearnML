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
| Not n -> not (eval n)
| AndAlso(e1,e2) -> eval e1 && eval e2
| OrElse(e1,e2) -> eval e1 || eval e2
| Imply(e1,e2) -> if eval e1 then eval e2  else true
| Equal(e1,e2) -> let rec ex = fun n -> match n with
            | Num k -> k
            | Plus (v1,v2) ->ex v1 + ex v2
            | Minus (v1,v2) -> ex v1 - ex v2
            in if ex e1 = ex e2 then true else false
