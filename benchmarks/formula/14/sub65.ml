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
    | NOT (form2) ->
        not (eval(form2))
    | ANDALSO (form1, form2) ->
       (eval (form1)) && (eval (form2))
    | ORELSE (form1, form2) ->
      eval(form1) || eval (form2)
    | IMPLY (form1, form2) ->
      if eval(form1) = true && eval(form2) = false then false
			else true		
		| LESS (exp1, exp2) ->
			let rec cal exp =
				match exp with
				| NUM (exp) -> exp
				| PLUS (exp1, exp2) -> cal(exp1) + cal(exp2)
				| MINUS (exp1, exp2) -> cal(exp1) - cal(exp2) in
			if cal(exp1) < cal(exp2) then true
			else false			
    | TRUE -> true
		| FALSE -> false;;

