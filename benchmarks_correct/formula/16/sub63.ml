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
	| Not (nf) -> not(eval (nf))
	| AndAlso (nf1, nf2) -> (eval (nf1)) && (eval (nf2))
	| OrElse (nf1, nf2) -> (eval (nf1)) || (eval (nf2))
	| Imply (nf1, nf2) -> not(eval (nf1)) || (eval (nf2))
	| Equal (nf1, nf2) -> let rec calnum = fun f1 -> match f1 with
	| Num n -> n
	| Plus (n1, n2) -> (calnum (n1)) + (calnum (n2))
	| Minus (n1, n2) -> (calnum (n1)) - (calnum (n2))
	in (calnum (nf1)) = (calnum (nf2));;
 
