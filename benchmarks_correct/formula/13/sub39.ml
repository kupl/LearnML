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

let rec makeexpr ex =
	match ex with
	| NUM i -> i
	| PLUS (ex1, ex2) -> (makeexpr ex1) + (makeexpr ex2)
	| MINUS (ex1, ex2) -> (makeexpr ex1) - (makeexpr ex2)

let rec eval f =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT f1 -> not (eval f1)
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f1, f2) -> (eval f1) || (eval f2)
	| IMPLY (f1, f2) ->
		if ((eval f2) = true) && ((eval f2) == true) then true
		else if ((eval f1) = false) then true
		else false
	| LESS (ex1, ex2) -> (makeexpr ex1) < (makeexpr ex2)


