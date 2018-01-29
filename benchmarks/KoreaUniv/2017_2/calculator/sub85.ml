
(*problem 5*)
type exp =X |INT of int |ADD of exp*exp |SUB of exp*exp
			 |MUL of exp*exp	|DIV of exp*exp |SIGMA of exp*exp*exp 

let rec sigma : exp->int->int = fun exp n ->
	match exp with
			|X -> n
			|INT a -> a
			|ADD (a,b) -> (sigma a n)+(sigma b n)
			|SUB (a,b) -> (sigma a n)-(sigma b n)
			|MUL (a,b) -> (sigma a n)*(sigma b n)
			|DIV (a,b) -> (sigma a n)/(sigma b n)
;;

let rec calculator : exp->int = fun e->
	match e with 
	|INT a -> a
	|ADD (a,b) -> (calculator a)+(calculator b)
	|SUB (a,b) -> (calculator a)-(calculator b)
	|MUL (a,b) -> (calculator a)*(calculator b)
	|DIV (a,b) -> (calculator a)/(calculator b)
	|SIGMA (exp1,exp2,exp3) ->
	let int1 = calculator exp1 in(
		let int2 = calculator exp2 in(
		if int1=int2 then sigma exp3 int1
		else (sigma exp3 int1) + (calculator (SIGMA(INT (int1+1),INT int2, exp3)))
		)
	)
;;
