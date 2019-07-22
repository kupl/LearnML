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

let rec eval(f:formula) =
	let rec cal(a:expr) =
		match a with
		| NUM(k) -> k
		| PLUS(b, c) -> cal(b) + cal(c)
		| MINUS(b, c) -> cal(b) - cal(c)
	in

	match f with
	|TRUE -> true
	|FALSE -> false
	|LESS(a,b) -> if cal(a)<cal(b) then true else false
	|NOT(a) -> not(eval(a))
	|ANDALSO(a,b) -> eval(a) && eval(b)
	|ORELSE(a,b) -> eval(a) || eval(b)
	|IMPLY(a,b) -> (not(eval(a))) || eval(b)

