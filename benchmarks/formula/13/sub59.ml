type formula=
    TRUE
	|FALSE
	|NOT of formula
	|ANDALSO of formula*formula
	|ORELSE of formula*formula
	|IMPLY of formula*formula
	|LESS of expr*expr
and expr=
    |NUM of int
	|PLUS of expr*expr
	|MINUS of expr*expr

let rec expr_eval:expr->int=
    fun ex->
	    match ex with
		|NUM n -> n
		|PLUS (ln,rn) -> (expr_eval ln)+(expr_eval rn)
	    |MINUS (ln,rn)-> (expr_eval ln)-(expr_eval rn)

let rec eval:formula->bool=
    fun fo->
	    match fo with
		| TRUE -> true
		| FALSE -> false
		| NOT f -> not((eval f))
	    | ANDALSO (lf,rf) -> (eval lf)&&(eval rf)
		| ORELSE (lf,rf) -> (eval lf)||(eval rf)
		| IMPLY (lf,rf) -> not((eval lf))||(eval rf)
		| LESS (ln,rn) -> (expr_eval ln)<(expr_eval rn)
