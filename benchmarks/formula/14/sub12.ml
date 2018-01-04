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
;;

let rec eval form =
	match form with
	| TRUE -> true
	| FALSE -> false
	| NOT first -> if eval first then false else true
	| ANDALSO (first, second) -> (eval first) && (eval second)
	| ORELSE (first, second) -> (eval first) || (eval second)
	| IMPLY (first, second) -> eval (ORELSE (NOT first, second))
	| LESS (first, second) -> let rec eval2 exp =
		match exp with
		| NUM val1 -> val1
		| PLUS (val1, val2) -> (eval2 val1) + (eval2 val2)
		| MINUS (val1, val2) -> (eval2 val1) - (eval2 val2)
		in
		(eval2 first) < (eval2 second)
;;


