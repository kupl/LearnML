type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr;;

type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr;;

let rec evalexpr e =
	match e with
	|NUM t -> t
	|PLUS(e1,e2) -> evalexpr(e1) + evalexpr(e2)
	|MINUS(e1,e2) -> evalexpr(e1) - evalexpr(e2);;

let rec eval f =
	match f with
	|TRUE -> true
	|FALSE -> false
	|NOT tail -> if eval(tail)==true then false else true
	|ANDALSO(t1, t2) -> if eval(t1) && eval(t2) then true else false
	|ORELSE(t1, t2) -> if eval(t1) || eval(t2) then true else false
	|IMPLY(t1, t2) -> if eval(t1) && (not(eval(t2))) then false else true
	|LESS(e1, e2) -> evalexpr(e1) < evalexpr(e2);;