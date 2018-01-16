type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
;;

type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
;;

let rec toNumber e =
	match e with NUM a -> a
		| PLUS (e1, e2) -> (toNumber e1) + (toNumber e2)
		| MINUS (e1, e2) -> (toNumber e1) - (toNumber e2)
;;	

let rec eval f =
	match f with TRUE -> true
		| FALSE -> false
		| NOT a -> not((eval a))
		| ANDALSO (a, b) -> (eval a) && (eval b)
		| ORELSE (a, b) -> (eval a) || (eval b)
		| IMPLY (a, b) -> not (eval a) || (eval b)
		| LESS (a, b) -> ( (toNumber a) < (toNumber b) )
;;
