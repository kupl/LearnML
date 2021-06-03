type formula =	  True
		| False
		| Not of formula
		| AndAlso of formula * formula
		| OrElse of formula * formula
		| Imply of formula * formula
		| Equal of exp * exp
and exp =	  Num of int
	 	| Plus of exp * exp
		| Minus of exp * exp

let rec eval_exp: exp -> int = fun(exp_to_evaluate)
                                -> (
                                   match exp_to_evaluate with
                                   Num(n) -> n
                                  |Plus(exp1_, exp2_) ->
                                        eval_exp(exp1_) + eval_exp(exp2_)
                                  |Minus(exp1_, exp2_) ->
                                        eval_exp(exp1_)  - eval_exp(exp2_)
                                   )

let rec eval: formula -> bool = fun(form)
-> (
	match form with
		  True -> true
		| False -> false
		| Not (form_) ->
			 (
			  if(eval(form_)) then false
			  else true
			 )
		| AndAlso(form1,form2) -> eval(form1) && eval(form2)
		| OrElse(form1,form2) -> eval(form1) || eval(form2)
		| Imply(form1,form2) -> 
			(
			  if(eval(form1))
				then( eval(form2) )
				else ( true ) 	
			)
		| Equal(exp1, exp2) -> 
			(
			  (eval_exp(exp1) = eval_exp(exp2))

			)

   )

