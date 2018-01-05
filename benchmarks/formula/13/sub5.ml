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
	let rec eval_expr expr =
		match expr with
		NUM (i) -> i
		| PLUS (e1, e2) -> (eval_expr e1) + (eval_expr e2)
		| MINUS (e1, e2) -> (eval_expr e1) - (eval_expr e2)
	in
	match formula with
		TRUE -> true
		| FALSE -> false
		| NOT (f) -> not (eval f)
		| ANDALSO (f1, f2) ->
			if (eval f1) then (eval f2)
			else false
		| ORELSE (f1, f2) ->
			if (eval f1) then true
			else (eval f2)
		| IMPLY (f1, f2) ->
			if (eval f1) then (eval f2)
			else true
		| LESS (e1, e2) ->
			(eval_expr e1) < (eval_expr e2)
