
type formula = TRUE
			|	FALSE
			|	NOT of formula
			|	ANDALSO of formula * formula
			|	ORELSE of formula * formula
			|	IMPLY of formula * formula
			|	LESS of expr * expr
and expr = NUM of int
		|	PLUS of expr * expr
		|	MINUS of expr * expr
;;

let rec eval form =
	let rec int_of_expr expr_in =
		match expr_in with
	|	NUM x -> x
	|	PLUS (expr1, expr2) -> int_of_expr(expr1) + int_of_expr(expr2)
	|	MINUS (expr1, expr2) -> int_of_expr(expr1) - int_of_expr(expr2)
	in
	match form with
	|	TRUE -> true
	|	FALSE -> false
	|	NOT form1 -> not (eval form1)
	|	ANDALSO (form1, form2) -> (eval form1) && (eval form2)
	|	ORELSE (form1, form2) -> (eval form1) || (eval form2)
	|	IMPLY (form1, form2) -> not (eval form1) || (eval form2)
	|	LESS (expr1, expr2) -> int_of_expr(expr1) < int_of_expr(expr2)
	;;

(* exercise test
Printf.printf "eval : %b\n" (eval (NOT(LESS(NUM 10, NUM 2))));;
exercise *)
