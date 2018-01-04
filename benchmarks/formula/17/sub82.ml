type formula =	  TRUE
		| FALSE
		| NOT of formula
		| ANDALSO of formula * formula
		| ORELSE of formula * formula
		| IMPLY of formula * formula
		| LESS of expr * expr
and expr =	  NUM of int
	 	| PLUS of expr * expr
		| MINUS of expr * expr

let rec eval_expr: expr -> int = fun(expr_to_evaluate)
                                -> (
                                   match expr_to_evaluate with
                                   NUM(n) -> n
                                  |PLUS(expr1_, expr2_) ->
                                        eval_expr(expr1_) + eval_expr(expr2_)
                                  |MINUS(expr1_, expr2_) ->
                                        eval_expr(expr1_)  - eval_expr(expr2_)
                                   )

let rec eval: formula -> bool = fun(form)
-> (
	match form with
		  TRUE -> true
		| FALSE -> false
		| NOT (form_) ->
			 (
			  if(eval(form_)) then false
			  else true
			 )
		| ANDALSO(form1,form2) -> eval(form1) && eval(form2)
		| ORELSE(form1,form2) -> eval(form1) || eval(form2)
		| IMPLY(form1,form2) -> 
			(
			  if(eval(form1))
				then( eval(form2) )
				else ( true ) 	
			)
		| LESS(expr1, expr2) -> 
			(
			  (eval_expr(expr1) < eval_expr(expr2))

			)

   )

