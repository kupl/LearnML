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

let rec sub_fun : exp -> int
= fun e ->
	match e with
	| Num i -> i
	| Plus (e1, e2) -> (sub_fun e1) + (sub_fun e2)
	| Minus (e1, e2) -> (sub_fun e1) - (sub_fun e2)

let rec eval : formula -> bool
= fun f ->
	match f with
	| True -> true
	| False -> false
	| Not a -> if eval a then false else true
	| AndAlso (a1, a2) -> if eval a1 && eval a2 then true else false
	| OrElse (a1, a2) -> if eval a1 || eval a2 then true else false
	| Imply (a1, a2) -> if eval a1 && eval(Not a2) then false else true
	| Equal (e1, e2) -> if (sub_fun e1) = (sub_fun e2) then true else false


