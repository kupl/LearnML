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
= fun f -> 
match f with
	True -> true
|	False -> false
|	Not f -> not (eval f)
|	AndAlso (f1, f2) -> eval f1 && eval f2
| OrElse (f1, f2) -> eval f1 || eval f2
|	Imply (f1, f2) -> if eval f1 = true && eval f2 = false then false else true
|	Equal (e1, e2) ->
		let rec evalNum : exp -> int
		= fun ex ->
		match ex with
			Num n -> n
		|	Plus (n1, n2) -> evalNum n1 + evalNum n2
		| Minus (n1, n2) -> evalNum n1 - evalNum n2
		in
		if evalNum e1 = evalNum e2 then true else false
