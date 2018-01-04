(* 2008-11874 EXERCISE 6 *)

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
	let rec cal e =
		match e with
			| NUM(i) -> i
			| PLUS(e1,e2) -> cal(e1) + cal(e2)
			| MINUS(e1,e2) -> cal(e1) - cal(e2)
	in
	match f with
		| TRUE -> true
		| FALSE -> false
		| NOT(f1) -> not (eval f1)
		| ANDALSO(f1,f2) -> (eval f1) && (eval f2)
		| ORELSE(f1,f2) -> (eval f1) || (eval f2)
		| IMPLY(f1,f2) -> not (((eval f1)=true) && ((eval f2)=false)) 
			(* false only when f1 is true but f2 is false.*)
		| LESS(e1,e2) -> (cal e1) < (cal e2)