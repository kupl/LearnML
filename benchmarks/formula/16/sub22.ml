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

let rec cal : exp -> int
= fun c->
	match c with
	| Num i -> i
	| Plus (e1,e2) -> cal e1 + cal e2
	| Minus (e1,e2) -> cal e1 - cal e2

let rec eval : formula -> bool
= fun f ->
	match f with
	| True -> true
	| False -> false
	| Not a -> not (eval a)
	| AndAlso (a,b) -> eval a && eval b
	| OrElse (a,b) -> eval a || eval b
	|	Imply (a,b) -> not (eval a && not (eval b))
	| Equal (e1,e2) ->
		if cal e1 = cal e2 then true
		else false

		
	
