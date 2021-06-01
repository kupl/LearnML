type formula 	= True
				| False
				| Not of formula
				| AndAlso of formula * formula
				| OrElse of formula * formula
				| Imply of formula * formula
				| Equal of exp * exp
	and exp 	= Num of int
				| Plus of exp * exp
				| Minus of exp * exp

let rec calc : exp -> int = function exp ->
     match exp with
     | Num i -> i
     | Plus (exp1, exp2) -> calc exp1 + calc exp2
     | Minus (exp1, exp2) -> calc exp1 - calc exp2


let rec eval : formula -> bool = function formula ->
	match formula with
	| True -> true
	| False -> false
	| Not form1 -> not (eval form1)
	| AndAlso (form1, form2) -> (eval form1) && (eval form2)
	| OrElse (form1, form2) -> (eval form1) || (eval form2)
	| Imply (form1, form2) -> (not (eval form1)) || (eval form2)
	| Equal (exp1, exp2) -> if calc exp1 = calc exp2 then true else false
