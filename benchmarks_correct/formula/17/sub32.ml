(* 2012-11230 Kim sangmin *)

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

let rec eval : formula -> bool = fun form ->
	let rec eval_exp : exp -> int = fun exp ->
		match exp with
		| Num i -> i
		| Plus(i,j) -> eval_exp(i)+eval_exp(j)
		| Minus(i,j) -> eval_exp(i)-eval_exp(j)
	in
	match form with
	| True -> true
	| False -> false
	| Not i -> not(eval(i))
	| AndAlso(i,j) -> eval(i) && eval(j)
	| OrElse(i,j) -> eval(i) || eval(j)
	| Imply(i,j) ->	if(not(eval(i))) then true
					else if(eval(j)) then true
					else false
	| Equal(i,j) -> if(eval_exp(i) = eval_exp(j)) then true
				   else false


