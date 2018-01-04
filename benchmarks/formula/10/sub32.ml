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

let rec eval_expr expr_arg =
 match expr_arg with
 (NUM n) -> n
 |(PLUS (expr1, expr2)) -> ((eval_expr expr1) + (eval_expr expr2))
 |(MINUS (expr1, expr2)) -> ((eval_expr expr1) - (eval_expr expr2))

let rec eval fmula =
 match fmula with
 TRUE -> true
 |FALSE -> false
 |(NOT f1) -> (not (eval f1))
 |(ANDALSO (f1, f2)) -> ((eval f1) && (eval f2))
 |(ORELSE (f1, f2)) -> ((eval f1) || (eval f2))
 |(IMPLY (f1, f2)) -> (if (eval f1) then (eval f2) else true)
 |(LESS (expr1, expr2)) -> ((eval_expr expr1) < (eval_expr expr2))


