type formula = TRUE
						 | FALSE
						 | NOT of formula
						 | ANDALSO of formula * formula
						 | ORELSE of formula * formula
						 | IMPLY of formula * formula
						 | LESS of expr * expr
and expr = NUM of int
				 | PLUS of expr * expr
				 | MINUS of expr * expr

let rec eval f =
	let rec cal exp =
		match exp with
		| NUM n -> n
		| PLUS (exp1, exp2) -> ((cal exp1) + (cal exp2))
		| MINUS (exp1, exp2) -> ((cal exp1) - (cal exp2)) in
			(match f with
			| TRUE -> true
			| FALSE -> false
			| NOT p -> (not (eval p))
			| ANDALSO (p, q) ->
				(if (((eval p) = true) && ((eval q) = true)) then (true)
				 else (false))
			| ORELSE (p, q) ->
				if (((eval p) = false) && ((eval q) = false)) then (false)
		 		else (true)
			| IMPLY (p, q) ->
				if (((eval p) = true) && ((eval q) = false)) then (false)
				else (true)
			| LESS (exp1, exp2) ->
				if ((cal exp1) < (cal exp2)) then (true)
				else (false))

