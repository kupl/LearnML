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

let rec eval : formula -> bool = fun f ->
	let rec expr_int : expr -> int = fun g ->
		match g with
		| NUM x -> x
		| PLUS (x,y) -> (expr_int x) + (expr_int y)
		| MINUS (x,y) -> (expr_int x) - (expr_int y)
	in match f with
	| TRUE -> true
	| FALSE -> false
	| NOT x -> if (eval x) then false
		   else true
	| ANDALSO (x,y) -> if (eval x) && (eval y) then true
			   else false
	| ORELSE (x,y) -> if (eval x) || (eval y) then true
			  else false
	| IMPLY (x,y) -> if (eval x) && (eval (NOT y)) then false
			 else true
	| LESS (a, b) -> (expr_int a) < (expr_int b)
