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



let rec eval input_formula =
	let 
		rec eval_exp input_exp =
			match input_exp with
			| Num(i) -> i
			| Plus(e1,e2) -> eval_exp(e1)+eval_exp(e2)
			| Minus(e1,e2) -> eval_exp(e1)-eval_exp(e2)
	in
		match input_formula with
		| True -> true
		| False -> false
		| Not(f) -> not ( eval(f) )
		| AndAlso(f1,f2) -> 
			if eval(f1) = false then false
			else if eval(f2) = false then false
			else true
		| OrElse(f1,f2) -> 
			if eval(f1) = true then true
			else if eval(f2) = true then true
			else false
		| Imply(f1,f2) -> 
			if eval(f1) = false then true
			else if eval(f2) = true then true
			else false
		| Equal(e1,e2) -> eval_exp(e1) = eval_exp(e2) 


