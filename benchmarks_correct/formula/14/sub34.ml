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

let rec cal e =
	match e with 
	| Num i -> i
	| Plus (e1, e2) -> (cal e1) + (cal e2)
	| Minus (e1, e2) -> (cal e1) - (cal e2)

		
let rec eval f =
	match f with
	| True -> true
	| False -> false
	| Not f1 -> not (eval f1) 
	| AndAlso (f1 , f2) -> (eval f1)&&(eval f2)
	| OrElse (f1, f2) -> (eval f1)||(eval f2)
	| Imply (f1, f2) -> if (eval f1) = true then (eval f2)
						else true
	| Equal (e1, e2) -> if (cal e1) = (cal e2) then true
						else false



