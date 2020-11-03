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

let rec evalExpr (ex: exp): int =
	match ex with
	| Num i -> i
	| Plus (e1, e2) -> (evalExpr e1) + (evalExpr e2)
	| Minus (e1, e2) -> (evalExpr e1) - (evalExpr e2)

let rec evalForm (fm: formula): formula =
	match fm with
	| True -> True
	| False -> False
	| Not f1 -> 
		if (evalForm f1) = True then False
		else True
	| AndAlso (f1, f2) -> 
		if ((evalForm f1 = True) && (evalForm f2 = True)) then True
		else False
	| OrElse (f1, f2) ->
		if ((evalForm f1 = True) || (evalForm f2 = True)) then True
		else False
	| Imply (f1, f2) ->
		if ((evalForm f1 = True) && (evalForm f2 = False)) then False
		else True
	| Equal (e1, e2) -> 
		if ((evalExpr e1) = (evalExpr e2)) then True
		else False


let eval (fm: formula): bool =
	match (evalForm fm) with
	| True -> true
	| False -> false 
	| _ -> false
