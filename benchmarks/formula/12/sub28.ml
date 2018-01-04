type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr

and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec eval fml =
	let rec get exp = 
		match exp with
		| NUM i -> i
		| PLUS (exp1, exp2) -> (get exp1) + (get exp2)
		| MINUS (exp1, exp2) -> (get exp1) - (get exp2)
	in

	match fml with
	| TRUE -> true
	| FALSE -> false
	| NOT fml1 -> not (eval fml1)
	| ANDALSO (fml1, fml2) -> (eval fml1) && (eval fml2)
	| ORELSE (fml1, fml2) -> (eval fml1) || (eval fml2)
	| IMPLY (fml1, fml2) -> 
		if (eval fml2) then true
		else if (eval fml1) then false
		else true
	| LESS (exp1, exp2) -> (get exp1) < (get exp2)
;;
