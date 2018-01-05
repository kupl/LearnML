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

let rec evalExpr (ex: expr): int =
	match ex with
	| NUM i -> i
	| PLUS (e1, e2) -> (evalExpr e1) + (evalExpr e2)
	| MINUS (e1, e2) -> (evalExpr e1) - (evalExpr e2)

let rec evalForm (fm: formula): formula =
	match fm with
	| TRUE -> TRUE
	| FALSE -> FALSE
	| NOT f1 -> 
		if (evalForm f1) = TRUE then FALSE
		else TRUE
	| ANDALSO (f1, f2) -> 
		if ((evalForm f1 = TRUE) && (evalForm f2 = TRUE)) then TRUE
		else FALSE
	| ORELSE (f1, f2) ->
		if ((evalForm f1 = TRUE) || (evalForm f2 = TRUE)) then TRUE
		else FALSE
	| IMPLY (f1, f2) ->
		if ((evalForm f1 = TRUE) && (evalForm f2 = FALSE)) then FALSE
		else TRUE
	| LESS (e1, e2) -> 
		if ((evalExpr e1) < (evalExpr e2)) then TRUE
		else FALSE


let eval (fm: formula): bool =
	match (evalForm fm) with
	| TRUE -> true
	| FALSE -> false 
	| _ -> false
