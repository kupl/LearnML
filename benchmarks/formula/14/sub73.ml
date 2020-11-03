
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


let rec eval form = 
	match form with
	| True -> true
	| False -> false
	| Not form1 -> not(eval(form1))
	| AndAlso  (form1,form2) ->  eval(form1) && eval(form2)
	| OrElse (form1,form2) ->  eval(form1) || eval(form2)
	| Imply (form1,form2) ->  if eval(form1)=true && eval(form2)=false then false  else true
	| Equal (exp1,exp2) -> _exp(exp1) = _exp(exp2)
and _exp __exp =
  	match __exp with
  	| Num n -> n
 	| Plus (exp1,exp2) -> _exp(exp1) + _exp(exp2)
 	| Minus (exp1,exp2) -> _exp(exp1) - _exp(exp2)