type formula = True
	| False
	|	Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec eval (f : formula) : bool =
	match f with
		True -> true
	| False -> false
	| Not p -> 
		if (eval p) then false
		else true
	| AndAlso (p, q) ->
		if (eval p) then (eval q)
		else false
	| OrElse (p, q) ->
		if (eval p) then true
		else (eval q)
	| Imply (p, q) ->
		if (eval p) then (eval q)
		else true
	| Equal (x, y) ->
		let rec exp_eval (e : exp) : int =
			match e with
				Num n -> n
			| Plus (e1, e2) -> (exp_eval e1) + (exp_eval e2)
			| Minus (e1, e2) -> (exp_eval e1) - (exp_eval e2)
		in
		(exp_eval x) = (exp_eval y)
