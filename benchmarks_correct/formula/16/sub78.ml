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

let rec eval_exp : exp -> int
= fun exp ->
	match exp with
	| Num n ->	n
	| Plus (n1, n2) ->	(eval_exp (n1)) + (eval_exp (n2))
	| Minus (n1, n2) ->	(eval_exp (n1)) - (eval_exp (n2))

let rec eval : formula -> bool
= fun f -> 
	match f with
	| True ->	true
	| False ->	false
	| Not (f1) ->	not (eval f1)
	| AndAlso (f1, f2) ->	(eval f1) && (eval f2)
	| OrElse (f1, f2) ->	(eval f1) || (eval f2)
	| Imply (f1, f2) ->
		begin
		match (eval f1, eval f2) with
		| (true, false) ->	false
		| _ ->	true
		end
	| Equal (e1, e2) ->
		if (eval_exp e1) = (eval_exp e2) then true else false
