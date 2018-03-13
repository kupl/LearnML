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
	let rec number : exp -> int = fun e ->
	match e with
	| Num(n) -> n
	| Plus(e1, e2) -> number e1 + number e2
	| Minus(e1, e2) -> number e1 - number e2
	in
	match f with
	| True -> true
	| False -> false
	| Not(f1) -> not (eval f1)
	| AndAlso(f1,f2) -> eval f1&& eval f2
	| OrElse(f1, f2) -> eval f1|| eval f2
	| Imply(f1, f2) -> if eval f1 = true && eval f2 = false then false else true
	| Equal(e1,e2) -> if number e1 = number e2 then true else false




	


