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

let rec eval f=
	let rec calculate exp=
		match exp with
			| NUM(i) -> i
			| PLUS(e1, e2) -> (calculate e1)+(calculate e2)
			| MINUS(e1, e2) ->(calculate e1)-(calculate e2)
	in
	match f with
		| FALSE ->false
		| TRUE -> true
		| NOT(f1) -> not (eval f1)
		| ANDALSO(f1, f2) ->(eval f1)&&(eval f2)
		| ORELSE(f1, f2) -> (eval f1) || (eval f2)
		| IMPLY (f1, f2) -> (eval f2)||((not (eval f2))&&(not (eval f1)))
		| LESS (e1, e2) -> if (calculate e1)<(calculate e2) then true
											else false
											