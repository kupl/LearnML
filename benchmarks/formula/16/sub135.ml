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

let rec evall : exp -> int
= fun e -> match e with
		| Num(a) -> a
		| Plus(a,b) -> evall(a)+evall(b)
		| Minus(a,b) -> evall(a)-evall(b)

let rec eval : formula -> bool
= fun f -> match f with
		| True -> true
		| False -> false
		| Not(a) -> if eval(a) then false else true
		| AndAlso(a,b) -> eval(a) && eval(b)
		| OrElse(a,b) -> eval(a) || eval(b)
		| Imply(a,b) -> eval(Not(AndAlso(a,Not(b))))
		| Equal(a,b) -> if evall(a)=evall(b) then true else false
