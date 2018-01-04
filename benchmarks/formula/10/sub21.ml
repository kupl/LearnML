
type formula = TRUE
			| FALSE
			| NOT of formula
			| ANDALSO of formula * formula
			| ORELSE of formula * formula
			| IMPLY of formula * formula
			| LESS of expr * expr
	and expr 	= NUM of int
				| PLUS of expr * expr
				| MINUS of expr * expr

;;
let rec expreval exp =
	match exp with
		NUM n -> n
		| PLUS (n1,n2) -> (expreval n1) + (expreval n2)
		| MINUS (n1,n2) -> (expreval n1) - (expreval n2)
;;
let rec eval form =
	match form with
		TRUE -> true
		| FALSE -> false
		| NOT f -> if (eval f) then false else true
		| ANDALSO (f1,f2) -> ((eval f1) && (eval f2))
		| ORELSE (f1,f2) -> ((eval f1) || (eval f2))
		| IMPLY (f1,f2) -> if (eval f1) && (eval f2) = false then false else true
		| LESS (e1,e2) -> ((expreval e1) < (expreval e2))
;;

