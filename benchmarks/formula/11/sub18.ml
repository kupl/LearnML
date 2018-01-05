(*
2008-12155
±èÂùÈ£
*)

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

let rec eval pro =
	let rec value exp =
		match exp with
		| NUM n -> n
		| PLUS(x, y) -> (value x) + (value y)
		| MINUS(x, y) -> (value x) - (value y)
	in

	match pro with
	| TRUE -> true
	| FALSE -> false
	| NOT form -> (if (eval form) = true then false
		else true)
	| ANDALSO(x, y) -> (if (eval x) = false then false
		else if (eval x) = true & (eval y) = true then true
		else false)
	| ORELSE(x, y) -> (if (eval x) = true then true
		else if (eval x) = false & (eval y) = false then false
		else true)
	| IMPLY(x, y) -> (if (eval x) = false then true
		else if (eval x) = true & (eval y) = true then true
		else false)
	| LESS(x, y) -> (if (value x) < (value y) then true
		else false)