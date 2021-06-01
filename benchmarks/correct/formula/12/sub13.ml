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

let rec eval formula =
	let rec evalExpr exp =
		match exp with
		| Num e -> e
		| Plus (e1, e2) -> evalExpr e1 + evalExpr e2
		| Minus (e1, e2) -> evalExpr e1 - evalExpr e2
	in
	match formula with
	| True -> true
	| False -> false
	| Not f -> if (eval f = true) then false
				else true
	| AndAlso (f1, f2) -> if (eval f1 = true && eval f2 = true) then true
						else false
	| OrElse (f1, f2) -> if (eval f1 = false && eval f2 = false) then false
						else true
	| Imply (f1, f2) -> if (eval f1 = true && eval f2 = false) then false
						else true
	| Equal (e1, e2) -> if (evalExpr e1 = evalExpr e2) then true
						else false