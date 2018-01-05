type formula = TRUE
	| FALSE
	|	NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec eval (f : formula) : bool =
	match f with
		TRUE -> true
	| FALSE -> false
	| NOT p -> 
		if (eval p) then false
		else true
	| ANDALSO (p, q) ->
		if (eval p) then (eval q)
		else false
	| ORELSE (p, q) ->
		if (eval p) then true
		else (eval q)
	| IMPLY (p, q) ->
		if (eval p) then (eval q)
		else true
	| LESS (x, y) ->
		let rec expr_eval (e : expr) : int =
			match e with
				NUM n -> n
			| PLUS (e1, e2) -> (expr_eval e1) + (expr_eval e2)
			| MINUS (e1, e2) -> (expr_eval e1) - (expr_eval e2)
		in
		(expr_eval x) < (expr_eval y)
