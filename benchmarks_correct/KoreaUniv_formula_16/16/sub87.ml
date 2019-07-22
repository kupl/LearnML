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
= fun e ->
	match e with
		| Plus (a, b) -> evalexp a + evalexp b
		| Minus (a, b) -> evalexp a - evalexp b
		| Num a -> a

let rec eval : formula -> bool
= fun f ->
	match f with
		| True -> true
		| False -> false
		| Not (a) -> if eval a then false else true
		| AndAlso (a, b) -> if eval a&&eval b then true else false
		| OrElse (a, b) -> if eval a||eval b then true else false
		| Imply (a, b) -> if eval b||(eval a=eval b) then true else false
		| Equal (a, b) -> if evalexp a=evalexp b then true else false
