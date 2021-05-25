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
    | Not (form2) ->
        not (eval(form2))
    | AndAlso (form1, form2) ->
       (eval (form1)) && (eval (form2))
    | OrElse (form1, form2) ->
      eval(form1) || eval (form2)
    | Imply (form1, form2) ->
      if eval(form1) = true && eval(form2) = false then false
			else true		
		| Equal (exp1, exp2) ->
			let rec cal exp =
				match exp with
				| Num (exp) -> exp
				| Plus (exp1, exp2) -> cal(exp1) + cal(exp2)
				| Minus (exp1, exp2) -> cal(exp1) - cal(exp2) in
			if cal(exp1) = cal(exp2) then true
			else false			
    | True -> true
		| False -> false;;

