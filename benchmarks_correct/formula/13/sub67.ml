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
let rec nexpr(e) =
	match e with
	| NUM n -> n
	| PLUS(e1,e2) -> (nexpr(e1))+(nexpr(e2))
	| MINUS(e1,e2) -> (nexpr(e1))-(nexpr(e2))
let rec eval(f) =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT ff -> not (eval(ff))
	| ANDALSO (f1,f2) -> (eval(f1)) && (eval(f2))
	| ORELSE (f1,f2) -> (eval(f1)) || (eval(f2))
	| IMPLY (f1,f2) -> ((not(eval(f1))) || (eval(f2)))
	| LESS (e1,e2) -> (nexpr(e1)) < (nexpr(e2))
