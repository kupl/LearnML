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

let rec eval_expr x =
	match x with
	| (NUM n) -> n
	| (PLUS (a, b)) -> ((eval_expr a) + (eval_expr b))
	| (MINUS (a, b)) -> ((eval_expr a) - (eval_expr b))

let rec eval x =
	match x with
	| (NOT f) -> (not (eval f))
	| (ANDALSO (f1, f2)) -> ((eval f1) && (eval f2))
	| (ORELSE (f1, f2)) -> ((eval f1) || (eval f2))
	| (IMPLY (f1, f2)) -> ((not (eval f1)) || (eval f2))
	| (LESS (f1, f2)) -> ((eval_expr f1) < (eval_expr f2))
	| TRUE -> true
	| FALSE -> false
