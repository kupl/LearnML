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

let rec calc : exp -> int
= fun e ->
match e with
|Num(a) -> a
|Plus(a,b) -> (calc (a) + calc (b))
|Minus(a,b) -> (calc(a) - calc(b))

let rec eval : formula -> bool
= fun f ->
match f with
| True -> true
| False -> false
| Not(a) -> if(eval(a)) then false else true
| AndAlso(a,b) -> if(eval(a)&&eval(b)) then true else false
| OrElse(a,b) -> if(eval(a)||eval(b)) then true else false
| Imply(a,b) -> if(eval(a)=false) then true else eval(b) 
| Equal(a,b) -> if((calc a)=(calc b)) then true else false



