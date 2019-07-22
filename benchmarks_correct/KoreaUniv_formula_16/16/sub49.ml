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
	| Not f -> not (eval f)
	| AndAlso (a, b) -> if (eval a) = false then false else if (eval b) = false then false else true
	| OrElse (a, b) -> if (eval a) = true then true else if (eval b) = true then true else false
	| Imply (a, b) -> not (eval a) || (eval b)
	| Equal (a, b) ->
		let rec eval2 e = match e with
			| Num n -> n
			| Plus (a, b) -> eval2 a + eval2 b
			| Minus (a, b) -> eval2 a - eval2 b
		in eval2 a = eval2 b