type expr = NUM of int
					| PLUS of expr * expr
					| MINUS of expr * expr

type formula = TRUE
							| FALSE
							| NOT of formula
							| ANDALSO of formula * formula
							| ORELSE of formula * formula
							| IMPLY of formula * formula
							| LESS of expr * expr

let eval form =
	let rec exprTOINT expr =
		match expr with
		| NUM(a) -> a
		| PLUS(a,b) -> exprTOINT(a) + exprTOINT(b)
		| MINUS(a,b) -> exprTOINT(a) - exprTOINT(b) in
	let rec formTOBool form =
		match form with
		| TRUE -> true
		| FALSE -> false
		| NOT a -> not (formTOBool (a))
		| ANDALSO (a,b) -> formTOBool (a) && formTOBool (b)
		| ORELSE (a,b) -> formTOBool (a) || formTOBool (b)
		| IMPLY (a,b) -> if (formTOBool (a) == true && formTOBool (b) == false)
											then false else true
		| LESS (h,t) -> if (exprTOINT (h) < exprTOINT (t))
										then formTOBool(TRUE) else formTOBool(FALSE) in
	formTOBool form
	
