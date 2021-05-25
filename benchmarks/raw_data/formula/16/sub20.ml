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
| Not (True) -> false
| Not (False) -> true
| AndAlso (a, b) -> (eval a)&&(eval b)
| OrElse (a, b) -> (eval a)||(eval b) ;;

