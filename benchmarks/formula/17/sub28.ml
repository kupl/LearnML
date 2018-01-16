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

let rec cal = function
	| NUM x -> x
	| PLUS (x,y) -> cal(x) + cal(y)
	| MINUS (x,y) -> cal(x) - cal(y)

let rec eval = function
	| TRUE -> true
	| FALSE -> false
	| NOT form -> not(eval(form))
	| ANDALSO (form1, form2) -> eval(form1) && eval(form2)
	| ORELSE (form1, form2) -> eval(form1) || eval(form2)
	| IMPLY (form1, form2) ->
		if eval(form1) then eval(form2)
		else true
	| LESS (expr1, expr2) -> cal(expr1) < cal(expr2)



