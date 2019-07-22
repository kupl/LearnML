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

let rec eval formula =
	let rec evalExpr expr =
		match expr with
		| NUM e -> e
		| PLUS (e1, e2) -> evalExpr e1 + evalExpr e2
		| MINUS (e1, e2) -> evalExpr e1 - evalExpr e2
	in
	match formula with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> if (eval f = true) then false
				else true
	| ANDALSO (f1, f2) -> if (eval f1 = true && eval f2 = true) then true
						else false
	| ORELSE (f1, f2) -> if (eval f1 = false && eval f2 = false) then false
						else true
	| IMPLY (f1, f2) -> if (eval f1 = true && eval f2 = false) then false
						else true
	| LESS (e1, e2) -> if (evalExpr e1 < evalExpr e2) then true
						else false