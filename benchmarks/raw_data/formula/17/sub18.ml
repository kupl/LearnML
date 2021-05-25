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

let rec eval f =
	let rec cal exp =
		match exp with
		| Num n -> n
		| Plus (exp1, exp2) -> ((cal exp1) + (cal exp2))
		| Minus (exp1, exp2) -> ((cal exp1) - (cal exp2)) in
			(match f with
			| True -> true
			| False -> false
			| Not p -> (not (eval p))
			| AndAlso (p, q) ->
				(if (((eval p) = true) && ((eval q) = true)) then (true)
				 else (false))
			| OrElse (p, q) ->
				if (((eval p) = false) && ((eval q) = false)) then (false)
		 		else (true)
			| Imply (p, q) ->
				if (((eval p) = true) && ((eval q) = false)) then (false)
				else (true)
			| Equal (exp1, exp2) ->
				if ((cal exp1) = (cal exp2)) then (true)
				else (false))

