
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


let rec eval = fun formula ->
	let rec expr_eval expr =
		match expr with
		| NUM(i) -> i
		| PLUS(e1,e2) -> expr_eval(e1) + expr_eval(e2)
		| MINUS(e1,e2) -> expr_eval(e1) - expr_eval(e2)
	in

	match formula with
	| TRUE -> true
	| FALSE -> false
	| NOT(f) -> not (eval f)
	| ANDALSO(f1,f2) -> (eval f1)&&(eval f2)
	| ORELSE(f1,f2) -> (eval f1)||(eval f2)
	| IMPLY(f1,f2) -> 
		let f1_res = eval f1 in
		let f2_res = eval f2 in
		if f1_res=false then true
		else if f2_res=true then true
		else false
	| LESS(e1,e2) -> 
		let e1_res = expr_eval(e1) in
		let e2_res = expr_eval(e2) in
		e1_res < e2_res
