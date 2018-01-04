
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

let rec evalexpr x =
	match x with
	| NUM y -> y
	| PLUS (a, b) -> (evalexpr a) + (evalexpr b)
	| MINUS (a, b) -> (evalexpr a) - (evalexpr b)

let rec eval a = 
	match a with
	| FALSE -> false
	| TRUE -> true
	| NOT x -> not (eval x)
	| ANDALSO (x, y) -> (eval x) && (eval y)
	| ORELSE (x, y) -> (eval x) || (eval y)
	| IMPLY (x, y) -> (not (eval x)) || (eval y)
	| LESS (x, y) -> (evalexpr x) < (evalexpr y)

