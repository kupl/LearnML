
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


let rec eval = fun formula ->
	let rec exp_eval exp =
		match exp with
		| Num(i) -> i
		| Plus(e1,e2) -> exp_eval(e1) + exp_eval(e2)
		| Minus(e1,e2) -> exp_eval(e1) - exp_eval(e2)
	in

	match formula with
	| True -> true
	| False -> false
	| Not(f) -> not (eval f)
	| AndAlso(f1,f2) -> (eval f1)&&(eval f2)
	| OrElse(f1,f2) -> (eval f1)||(eval f2)
	| Imply(f1,f2) -> 
		let f1_res = eval f1 in
		let f2_res = eval f2 in
		if f1_res=false then true
		else if f2_res=true then true
		else false
	| Equal(e1,e2) -> 
		let e1_res = exp_eval(e1) in
		let e2_res = exp_eval(e2) in
		e1_res = e2_res
