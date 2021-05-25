type formula = True
	     | False
	     | Not of formula
	     | AndAlso of formula * formula
	     | OrElse of formula * formula
	     | Imply of formula * formula
	     | Equal of exp * exp
and exp = Num of int
	 | Plus of exp * exp
	 | Minus of exp * exp

let rec eval : formula -> bool = fun f ->
	let rec exp_int : exp -> int = fun g ->
		match g with
		| Num x -> x
		| Plus (x,y) -> (exp_int x) + (exp_int y)
		| Minus (x,y) -> (exp_int x) - (exp_int y)
	in match f with
	| True -> true
	| False -> false
	| Not x -> if (eval x) then false
		   else true
	| AndAlso (x,y) -> if (eval x) && (eval y) then true
			   else false
	| OrElse (x,y) -> if (eval x) || (eval y) then true
			  else false
	| Imply (x,y) -> if (eval x) && (eval (Not y)) then false
			 else true
	| Equal (a, b) -> (exp_int a) = (exp_int b)
