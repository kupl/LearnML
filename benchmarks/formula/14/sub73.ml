
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


let rec eval form = 
	match form with
	| TRUE -> true
	| FALSE -> false
	| NOT form1 -> not(eval(form1))
	| ANDALSO  (form1,form2) ->  eval(form1) && eval(form2)
	| ORELSE (form1,form2) ->  eval(form1) || eval(form2)
	| IMPLY (form1,form2) ->  if eval(form1)=true && eval(form2)=false then false  else true
	| LESS (expr1,expr2) -> _expr(expr1) < _expr(expr2)
and _expr __expr =
  	match __expr with
  	| NUM n -> n
 	| PLUS (expr1,expr2) -> _expr(expr1) + _expr(expr2)
 	| MINUS (expr1,expr2) -> _expr(expr1) - _expr(expr2)