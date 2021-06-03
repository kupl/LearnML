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

let rec eval : formula -> bool = fun f ->
	let rec cal_exp : exp -> int = fun e ->
		match e with
		| Num i -> i
		| Plus (e1,e2) -> cal_exp(e1) + cal_exp(e2)
		| Minus (e1,e2) -> cal_exp(e1) - cal_exp(e2)
	in

	match f with
	| True -> true
	| False -> false
	| Not s -> not (eval (s))
	| AndAlso (f1,f2) -> (eval (f1) && eval (f2))
	| OrElse (f1,f2) -> (eval (f1) || eval (f2))
	| Imply (f1,f2) -> (not (eval (f1)) || eval(f2))
	| Equal (e1,e2) -> 
		if(cal_exp (e1) = cal_exp (e2)) then true
		else false

(*
let a61 = eval True 
let a62 = eval False 
let a63 = eval (Not True) 
let a64 = eval (AndAlso (True, False)) 
let a65 = eval (OrElse (True, False)) 
let a66 = eval (Equal (Plus(Num 3, Num 4), Minus(Num 7, Num 8)))
*)
