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
	(* make eval : formula -> bool*)
let rec eval formula = 
	let rec eval_exp exp = 
		match exp with
		| Num i -> i
		| Plus (a, b) -> (eval_exp a) + (eval_exp b)
		| Minus (a, b) -> (eval_exp a) - (eval_exp b)
	in
	match formula with
	| True -> true
	| False -> false
	| Not fm -> not (eval fm)
	| AndAlso(fm1,fm2) -> (eval fm1)&&(eval fm2)
	| OrElse(fm1,fm2) -> (eval fm1)||(eval fm2)
	| Imply(fm1,fm2) -> (not (eval fm1))||(eval fm2)
	| Equal(exp1,exp2)
		 -> if((eval_exp exp1) = (eval_exp exp2)) then true
		 	else false

