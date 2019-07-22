(* 2006-11782 Song Young-chan, Hw2-3 True/False *)

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

let rec eval_expr expression = 
	match expression with
	  NUM(number) -> number
	| PLUS(num1, num2) -> ((eval_expr num1) + (eval_expr num2))
	| MINUS(num1, num2) -> ((eval_expr num1) - (eval_expr num2))

let rec eval input_formula = 
	match input_formula with
	  TRUE -> true
	| FALSE -> false
	| NOT(t_f) -> if ((eval t_f)=true) then false
		      else true
	| ANDALSO(t_f1, t_f2) -> ((eval t_f1) && (eval t_f2))
	| ORELSE(t_f1, t_f2) -> ((eval t_f1) || (eval t_f2))
	| IMPLY(t_f1, t_f2) -> if (((eval t_f1) = false) || ((eval t_f2) = true)) then true
			       else false
	| LESS(expr1, expr2) -> if ((eval_expr expr1) < (eval_expr expr2)) then true
				else false
