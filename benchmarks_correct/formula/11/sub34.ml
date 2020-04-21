(* 2009-13384, CHO Hyunik *)



type formula =	True | False 
		| Not of formula 
		| AndAlso of formula * formula 
		| OrElse of formula * formula
		| Imply of formula * formula
		| Equal of exp * exp
and exp =	Num of int
		| Plus of exp * exp
		| Minus of exp * exp



let rec eval formula1 =

(* LOCAL FUNCTION expCalculation : function that translate formula to integer *)
	let rec expCalculation exp1 =
		match exp1 with
		Num(a) -> a
		| Plus(a, b) -> (expCalculation a)+(expCalculation b)
		| Minus(a, b) -> (expCalculation a)-(expCalculation b) in


	match formula1 with
	True -> true
	| False -> false
	| Not(a) -> not(eval a)
	| AndAlso(a, b) -> (eval a)&&(eval b)
	| OrElse(a, b) -> (eval a)||(eval b)
	| Imply(a, b) -> (eval b)||(not(eval a))
	| Equal(a, b) -> (expCalculation a) = (expCalculation b)