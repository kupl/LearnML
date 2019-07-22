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

let rec cal e =
	match e with
	| NUM num -> num
	| PLUS (lexpr, rexpr) -> (cal lexpr) + (cal rexpr)
	| MINUS (lexpr, rexpr) -> (cal lexpr) - (cal rexpr)	

let rec eval f =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT form -> not (eval form)
	| ANDALSO (lform, rform) -> (eval lform) && (eval rform)
	| ORELSE (lform, rform) -> (eval lform) || (eval rform)
	| IMPLY (lform, rform) -> (not (eval lform)) || (eval rform)
	| LESS (lexpr, rexpr) -> (cal lexpr) < (cal rexpr)
