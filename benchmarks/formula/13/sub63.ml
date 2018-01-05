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

let rec eval f = 
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT (formula) -> not (eval formula)
	| ANDALSO (formula1, formula2) -> (eval formula1) && (eval formula2)
	| ORELSE (formula1, formula2) -> (eval formula1) || (eval formula2)
	| IMPLY (formula1, formula2) -> (not (eval formula1)) || (eval formula2)
	| LESS (expr1, expr2) -> (expr1 < expr2)

let rec cal e =
	match e with
	| NUM (x) -> x
	| PLUS (x, y) -> cal(x)+cal(y)
	| MINUS (x, y) -> cal(x)-cal(y)
