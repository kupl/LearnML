(* HW1 exercise6 2009-11697 Kim HyunJoon *)
(* Eval *)

type formula 	= True 
		| False 
		| Not of formula 
		| AndAlso of formula * formula
		| OrElse of formula * formula
		| Imply of formula * formula
		| Equal of exp * exp

and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec eval : formula -> bool = 
	fun input ->
	let rec value exp =
		match exp with
		| Num n -> n
		| Plus (exp1, exp2) -> (value exp1)+(value exp2)
		| Minus (exp1, exp2) -> (value exp1)-(value exp2)
	in
	match input with
	| True -> true
	| False -> false
	| Not f -> (not (eval f))
	| AndAlso (f1, f2) -> (eval f1) && (eval f2)
	| OrElse (f1, f2) -> (eval f1) || (eval f2)
	| Imply (f1, f2) -> (not (eval f1)) || (eval f2)
	| Equal (exp1, exp2) -> ((value exp1) = (value exp2))
