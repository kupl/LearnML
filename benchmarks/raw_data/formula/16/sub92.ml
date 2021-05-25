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
|True -> true
|False ->false
|Not(x) ->not(eval x)
|AndAlso(x,y) -> (eval x) && (eval y)
|OrElse(x,y)-> (eval x)||(eval y)
|Imply(x,y) -> not (eval x) || (eval y)
|Equal(x,y)-> let rec equality e=match e with 
		|Num(a)->a
		|Plus(a,b)->equality(a)+equality(b)
		|Minus(a,b)->equality(a)-equality(b)
		in if equality(x)=equality(y) then true else false
;;		
