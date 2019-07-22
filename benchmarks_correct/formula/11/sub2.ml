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



let rec eval input_formula =
	let 
		rec eval_expr input_expr =
			match input_expr with
			| NUM(i) -> i
			| PLUS(e1,e2) -> eval_expr(e1)+eval_expr(e2)
			| MINUS(e1,e2) -> eval_expr(e1)-eval_expr(e2)
	in
		match input_formula with
		| TRUE -> true
		| FALSE -> false
		| NOT(f) -> not ( eval(f) )
		| ANDALSO(f1,f2) -> 
			if eval(f1) = false then false
			else if eval(f2) = false then false
			else true
		| ORELSE(f1,f2) -> 
			if eval(f1) = true then true
			else if eval(f2) = true then true
			else false
		| IMPLY(f1,f2) -> 
			if eval(f1) = false then true
			else if eval(f2) = true then true
			else false
		| LESS(e1,e2) -> eval_expr(e1) < eval_expr(e2) 


