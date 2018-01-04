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

let rec numExpr ex = 
	match ex with 
	| NUM n -> n
	| PLUS (n1,n2) -> ((numExpr n1) + (numExpr n2))
	| MINUS (n1,n2) -> ((numExpr n1) - (numExpr n2))

let rec eval value = 
	match value with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> not (eval f)
	| ANDALSO (f1,f2) -> (&&) (eval f1) (eval f2)
	| ORELSE (f1,f2) -> (||) (eval f1) (eval f2)
	| IMPLY (f1,f2) -> (if (eval f1) = true && (eval f2) = false then false
							else true)
	| LESS (e1,e2) -> (if (numExpr e1) < (numExpr e2) then true
						else false)
