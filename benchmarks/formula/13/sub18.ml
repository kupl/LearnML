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
	(* make eval : formula -> bool*)
let rec eval formula = 
	let rec eval_expr expr = 
		match expr with
		| NUM i -> i
		| PLUS (a, b) -> (eval_expr a) + (eval_expr b)
		| MINUS (a, b) -> (eval_expr a) - (eval_expr b)
	in
	match formula with
	| TRUE -> true
	| FALSE -> false
	| NOT fm -> not (eval fm)
	| ANDALSO(fm1,fm2) -> (eval fm1)&&(eval fm2)
	| ORELSE(fm1,fm2) -> (eval fm1)||(eval fm2)
	| IMPLY(fm1,fm2) -> (not (eval fm1))||(eval fm2)
	| LESS(exp1,exp2)
		 -> if((eval_expr exp1) < (eval_expr exp2)) then true
		 	else false

