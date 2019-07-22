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
	
	let rec eval_expr e =
		match e with
		| NUM i -> i
		| PLUS (i1, i2) -> (eval_expr i1) + (eval_expr i2)
		| MINUS (i1, i2) -> (eval_expr i1) - (eval_expr i2)
	in

	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT f1 -> not (eval f1)
	| ANDALSO (f1, f2) -> eval f1 && eval f2
	| ORELSE (f1, f2) -> eval f1 || eval f2
	| IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
	| LESS (e1, e2) -> if eval_expr e1 < eval_expr e2 then true else false

