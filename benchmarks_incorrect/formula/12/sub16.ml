(* ex6 *)
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
		 
let rec eval e = 

	let rec expEval exp = 
		match exp with
		  NUM i -> i
		| PLUS (e1, e2) -> (expEval e1) + (expEval e2)
		| MINUS (e1, e2) -> (expEval e1) - (expEval e2)
	in
	
	match e with
	  TRUE -> true
	| FALSE -> false
	| NOT f -> not (eval f)
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f1, f2) -> (eval f1) || (eval f2)
	| IMPLY (f1, f2) -> eval (ORELSE (f1, (NOT f2)))
	| LESS (e1, e2) -> (expEval e1) < (expEval e2)
	