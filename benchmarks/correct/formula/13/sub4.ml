type exp 	= Num of int
			| Plus of exp * exp
			| Minus of exp * exp

type formula 	= True
				| False
				| Not of formula
				| AndAlso of formula * formula
				| OrElse of formula * formula
				| Imply of formula * formula
				| Equal of exp * exp

let rec result_of_exp exp =
	match exp with
	| (Num v) -> v
	| (Plus (e1, e2)) -> (result_of_exp e1) + (result_of_exp e2)
	| (Minus (e1, e2)) -> (result_of_exp e1) - (result_of_exp e2)


let rec eval f =
	match f with
	| True -> true
	| False -> false
	| (Not f) -> not (eval f)
	| (AndAlso (f1, f2)) -> (eval f1) & (eval f2)
	| (OrElse (f1, f2)) -> (eval f1) || (eval f2)
	| (Imply (f1, f2)) -> if (eval f1)=false then true
						  else if (eval f2)=true then true
						  else false
	| (Equal (e1, e2)) -> (result_of_exp e1) = (result_of_exp e2)
