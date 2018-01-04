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

let rec eval k =
	let rec calc e =
		match e with
			| NUM(n) -> n
			| PLUS(a,b) -> (calc a) + (calc b)
			| MINUS(a,b) -> (calc a) - (calc b)
		in
	match k with
		| TRUE -> true
		| FALSE -> false
		| NOT(a) -> not (eval a)
		| ANDALSO(a,b) -> (eval a) && (eval b)
		| ORELSE(a,b) -> (eval a) || (eval b)
		| IMPLY(a,b) -> (not (eval a)) || (eval b)
		| LESS(a,b) -> (calc a) < (calc b)