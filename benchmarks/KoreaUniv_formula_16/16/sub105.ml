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
= fun f ->match f with |True->true|False->false |Not x -> if eval x = true then false else true | AndAlso(x,y)->if eval x = false then false else if eval y = false  then false else true | OrElse(x,y)->if eval x =true then true else if eval y = true  then true else false | Imply(x,y) -> if eval x = true&&eval y = false  then false else true | Equal(x,y)->let rec ev : exp->int = fun e -> match e with |Num n -> n |Plus(x,y) -> ev x + ev y |Minus(x,y)-> ev x - ev y in if ev x = ev y then true else false;;


