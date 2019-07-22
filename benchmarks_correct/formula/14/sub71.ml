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

let rec evalexpr e =
	match e with
	| NUM n -> n
	| PLUS (e0, e1) -> (evalexpr e0) + (evalexpr e1)
	| MINUS (e0, e1) -> (evalexpr e0) - (evalexpr e1)

let rec eval f =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT f0 -> not (eval f0)
	| ANDALSO (f0, f1) -> (eval f0) && (eval f1)
	| ORELSE (f0, f1) -> (eval f0) || (eval f1)
	| IMPLY (f0, f1) -> not (eval f0) || ((eval f0) && (eval f1))
	| LESS (e0, e1) -> (evalexpr e0) < (evalexpr e1)

