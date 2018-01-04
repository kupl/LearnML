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

let rec eval = fun f ->
	let rec calc = fun ex ->
		match ex with
			NUM(a) -> a
			|PLUS(a, b) -> (calc a) + (calc b)
			|MINUS(a,b) -> (calc a) - (calc b)
	in
	match f with
		TRUE -> true
		| FALSE -> false
		| NOT(x) -> not (eval x)
		| ANDALSO(x, y) -> (eval x) && (eval y)
		| ORELSE(x, y) -> (eval x) || (eval y)
		| IMPLY(x, y) -> (not (eval x)) || (eval y)
		| LESS(x, y) -> (calc x) < (calc y)
