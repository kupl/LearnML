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

let rec evale : expr -> int = fun exp ->
	match exp with
	| NUM n -> n
	| PLUS (exp1, exp2) -> evale(exp1) + evale(exp2)
	| MINUS (exp1, exp2) -> evale(exp1) - evale(exp2)

let rec eval : formula -> bool = fun f ->
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT f1 -> not(eval(f1))
	| ANDALSO (f1, f2) -> (if eval(f1) then eval(f2) else false)
	| ORELSE (f1, f2) -> (if eval(f1) then true else eval(f2))
	| IMPLY (f1, f2) -> (if (eval(f1) && (not (eval(f2)))) then false else true)
	| LESS (exp1, exp2) -> (evale(exp1) < evale(exp2))
