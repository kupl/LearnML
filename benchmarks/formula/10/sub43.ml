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

let rec eval f = match f
	with FALSE -> false
	| TRUE -> true
	| NOT f1 -> (not(eval f1))
	| ANDALSO (f1,f2) -> (eval f1) && (eval f2)
	| ORELSE (f1,f2) -> (eval f1) || (eval f2)
	| IMPLY (f1,f2) -> (not(eval f1)) || (eval f2)
	| LESS (e1,e2) -> (exeval e1) < (exeval e2)
and exeval e = match e
	with NUM i -> i
	| PLUS (e1,e2) -> (exeval e1)+(exeval e2)
	| MINUS (e1,e2) -> (exeval e1)-(exeval e2)

