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

let rec exp_eval : exp -> int
= fun e ->
	match e with
	| Num(n) -> n
	| Plus(n1,n2) -> exp_eval n1 + exp_eval n2
	| Minus(n1,n2) -> exp_eval n1 - exp_eval n2

let rec eval : formula -> bool
= fun f -> (* TODO *)
	match f with
	| True -> true
	| False -> false
	| Not(f1) -> if eval f1 = true then false else true
	| AndAlso(f1,f2) -> if eval f1 = true && eval f2 = true then true else false
	| OrElse(f1,f2) -> if eval f1 = false && eval f2 = false then false else true
	| Imply(f1,f2) ->
		if eval f1 = false then true
		else if eval f1 = true && eval f2 = true then true
		else false
	| Equal(exp1,exp2) ->
		if exp_eval exp1 = exp_eval exp2 then true
		else false


