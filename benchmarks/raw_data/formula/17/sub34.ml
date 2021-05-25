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

let rec exp_to_int : exp -> int = fun exp ->
	match exp with
	| Num x -> x
	| Plus (e1, e2) -> exp_to_int e1 + exp_to_int e2
	| Minus (e1, e2) -> exp_to_int e1 - exp_to_int e2

let rec eval : formula -> bool = fun formula ->
	match formula with
	| True -> true
	| False -> false
	| Not f -> if eval f = true then false
			   else true
	| AndAlso (f1, f2) -> if ((eval f1 = true) && (eval f2 = true)) then true
						  else false
	| OrElse (f1, f2) -> if ((eval f1 = false) && (eval f2 = false)) then false
						 else true
	| Imply (f1, f2) -> if eval f1 = false then true
						else if eval f2 = true then true
						else false
	| Equal (e1, e2) -> if exp_to_int e1 = exp_to_int e2 then true
					   else false

