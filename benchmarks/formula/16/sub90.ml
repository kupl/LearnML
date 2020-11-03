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
	| Not a -> if (eval a) then false else true
	| AndAlso (a,b) -> if (eval a)&&(eval b) then true else false
	| OrElse (a,b) -> if (eval a)||(eval b) then true else false
	| Imply (a,b) -> if not (eval a)||(eval b) then true else false
	|  Equal (a,b) -> let rec exp z = match z with
                       			| Num n-> n
                			| Plus(x,y) -> exp(x)+exp(y)
					| Minus(x,y) -> exp(x)-exp(y)
            in if (exp a)==(exp b) then true else false
;;





