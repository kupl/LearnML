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

let rec eval fml =
	let rec get exp = 
		match exp with
		| Num i -> i
		| Plus (exp1, exp2) -> (get exp1) + (get exp2)
		| Minus (exp1, exp2) -> (get exp1) - (get exp2)
	in

	match fml with
	| True -> true
	| False -> false
	| Not fml1 -> not (eval fml1)
	| AndAlso (fml1, fml2) -> (eval fml1) && (eval fml2)
	| OrElse (fml1, fml2) -> (eval fml1) || (eval fml2)
	| Imply (fml1, fml2) -> 
		if (eval fml2) then true
		else if (eval fml1) then false
		else true
	| Equal (exp1, exp2) -> (get exp1) = (get exp2)
;;
