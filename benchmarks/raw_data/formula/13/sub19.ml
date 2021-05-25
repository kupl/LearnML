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


let rec eval formula =
	let rec calc_exp exp = 
		match exp with
		| Num x -> x
		| Plus (x, y) -> (calc_exp x) + (calc_exp y)
		| Minus (x, y) -> (calc_exp x) - (calc_exp y) in
	
	match formula with
	| True -> true
	| False -> false
	| Not x -> if (eval x)=true then false
				else true
	| AndAlso (x, y) -> (eval x) && (eval y)
	| OrElse (x, y) -> (eval x) || (eval y)
	| Imply (x, y) -> if (eval x)=false then true
					else if (eval y)=true then true
					else false
	| Equal (x, y) -> if (calc_exp x)=(calc_exp y) then true
					else false

