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

let rec exprEval expr =
	match expr with
		| NUM n -> n
		| PLUS (a, b) -> (exprEval a) + (exprEval b)
		| MINUS (a, b) -> (exprEval a) - (exprEval b)

let rec eval formula =
	match formula with
		| TRUE -> true
		| FALSE -> false
		| NOT f -> not (eval f)
		| ANDALSO (a, b) -> (eval a) && (eval b)
		| ORELSE (a, b) -> (eval a) || (eval b)
		| IMPLY (a, b) -> (not (eval a)) || (eval b)
		| LESS (a, b) -> (exprEval a) < (exprEval b)
