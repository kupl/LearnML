type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp
and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec expToInt : exp -> int
= fun exp1 ->
	match exp1 with
		| Num(int1) -> int1
		| Plus(e1, e2) -> expToInt e1 + expToInt e2
		| Minus(e1, e2) -> expToInt e1 - expToInt e2

let rec eval : formula -> bool
= fun f -> 
	match f with
	| True -> true
	| False -> false
	| Not(formula) -> if eval formula = true then false else true
	| AndAlso(formula1, formula2) -> 
			if eval formula1 = true && eval formula2 = true then true else false
	| OrElse(formula1, formula2) ->
			if eval formula1 = false && eval formula2 = false then false else true
	| Imply(formula1, formula2) ->
			if eval formula1 = false then true
			else if eval formula2 = true then true
			else false
	| Equal(exp1, exp2) -> if expToInt exp1 = expToInt exp2 then true else false
