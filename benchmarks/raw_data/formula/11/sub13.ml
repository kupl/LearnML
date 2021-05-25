

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


let rec eval f =
	let rec cal e =
		match e with
		Num x -> x
		| Plus (x, y) -> (cal x) + (cal y)
		| Minus (x, y) -> (cal x) - (cal y)
	in

	match f with
	True -> true
	| False -> false
	| Not f -> if (eval f) then false else true
	| AndAlso (f1, f2) -> (eval f1)&&(eval f2)
	| OrElse (f1, f2) -> (eval f1)||(eval f2)
	| Imply (f1, f2) -> if (eval f1) = true && (eval f2) = false then
				false 
			    else true	
        | Equal (e1, e2) -> (cal e1) = (cal e2)

