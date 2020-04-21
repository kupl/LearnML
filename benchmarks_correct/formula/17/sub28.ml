type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec cal = function
	| Num x -> x
	| Plus (x,y) -> cal(x) + cal(y)
	| Minus (x,y) -> cal(x) - cal(y)

let rec eval = function
	| True -> true
	| False -> false
	| Not form -> not(eval(form))
	| AndAlso (form1, form2) -> eval(form1) && eval(form2)
	| OrElse (form1, form2) -> eval(form1) || eval(form2)
	| Imply (form1, form2) ->
		if eval(form1) then eval(form2)
		else true
	| Equal (exp1, exp2) -> cal(exp1) = cal(exp2)



