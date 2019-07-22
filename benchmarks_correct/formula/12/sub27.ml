type formula  = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr

	and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec calExpr exp =
	match exp with
	| NUM i -> i
	| PLUS (left, right) -> (calExpr left) + (calExpr right)
	| MINUS (left, right) -> (calExpr left) - (calExpr right)

let rec eval fml =
	match fml with
	| TRUE -> true
	| FALSE -> false
	| NOT subFml -> not (eval subFml)
	| ANDALSO (fml1, fml2) -> (eval fml1) && (eval fml2)
	| ORELSE (fml1, fml2)  -> (eval fml1) || (eval fml2)
	| IMPLY (fml1, fml2) 	-> if ((eval fml1)&&(eval fml2))|| (not (eval fml1)) then true else false
	| LESS (fml1, fml2)	-> if ((calExpr fml1) < (calExpr fml2)) then true else false 
