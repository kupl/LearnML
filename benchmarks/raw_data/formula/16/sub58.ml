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

let rec evalexp : exp -> int
= fun exp -> match exp with
| Num n -> n
| Plus (x, y) -> (evalexp x) + (evalexp y)
| Minus (x, y) -> (evalexp x) - (evalexp y)

let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Not a -> not (eval a)
| AndAlso (a, b) -> (eval a) && (eval b)
| OrElse (a, b) -> (eval a) || (eval b)
| Imply (a, b) -> (not (eval a)) || (eval b)
| Equal (x, y) -> (evalexp x) = (evalexp y)



