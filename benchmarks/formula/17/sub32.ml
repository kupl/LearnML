(* 2012-11230 Kim sangmin *)

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

let rec eval : formula -> bool = fun form ->
	let rec eval_expr : expr -> int = fun exp ->
		match exp with
		| NUM i -> i
		| PLUS(i,j) -> eval_expr(i)+eval_expr(j)
		| MINUS(i,j) -> eval_expr(i)-eval_expr(j)
	in
	match form with
	| TRUE -> true
	| FALSE -> false
	| NOT i -> not(eval(i))
	| ANDALSO(i,j) -> eval(i) && eval(j)
	| ORELSE(i,j) -> eval(i) || eval(j)
	| IMPLY(i,j) ->	if(not(eval(i))) then true
					else if(eval(j)) then true
					else false
	| LESS(i,j) -> if(eval_expr(i) < eval_expr(j)) then true
				   else false


