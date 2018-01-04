(* HW1 exercise6 2009-11697 Kim HyunJoon *)
(* Eval *)

type formula 	= TRUE 
		| FALSE 
		| NOT of formula 
		| ANDALSO of formula * formula
		| ORELSE of formula * formula
		| IMPLY of formula * formula
		| LESS of expr * expr

and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec eval : formula -> bool = 
	fun input ->
	let rec value exp =
		match exp with
		| NUM n -> n
		| PLUS (exp1, exp2) -> (value exp1)+(value exp2)
		| MINUS (exp1, exp2) -> (value exp1)-(value exp2)
	in
	match input with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> (not (eval f))
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f1, f2) -> (eval f1) || (eval f2)
	| IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
	| LESS (exp1, exp2) -> ((value exp1) < (value exp2))
