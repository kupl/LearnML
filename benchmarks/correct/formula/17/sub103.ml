type exp = Num of int
					| Plus of exp * exp
					| Minus of exp * exp

type formula = True
							| False
							| Not of formula
							| AndAlso of formula * formula
							| OrElse of formula * formula
							| Imply of formula * formula
							| Equal of exp * exp

let eval form =
	let rec expTOINT exp =
		match exp with
		| Num(a) -> a
		| Plus(a,b) -> expTOINT(a) + expTOINT(b)
		| Minus(a,b) -> expTOINT(a) - expTOINT(b) in
	let rec formTOBool form =
		match form with
		| True -> true
		| False -> false
		| Not a -> not (formTOBool (a))
		| AndAlso (a,b) -> formTOBool (a) && formTOBool (b)
		| OrElse (a,b) -> formTOBool (a) || formTOBool (b)
		| Imply (a,b) -> if (formTOBool (a) == true && formTOBool (b) == false)
											then false else true
		| Equal (h,t) -> if (expTOINT (h) = expTOINT (t))
										then formTOBool(True) else formTOBool(False) in
	formTOBool form
	
