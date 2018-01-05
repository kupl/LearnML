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

let rec evalexpr ex = 
	match ex with
		NUM(i) -> i
		| PLUS(e1, e2) -> ((evalexpr e1) + (evalexpr e2))
		| MINUS(e1, e2) -> ((evalexpr e1) - (evalexpr e2))

let rec eval form = 
	match form with
		TRUE -> true
		| FALSE -> false
		| NOT(f) -> (not (eval f))
		| ANDALSO(f, g) -> ((eval f) && (eval g))
		| ORELSE(f, g) -> ((eval f) || (eval g))
		| IMPLY(f, g) -> ((not (eval f)) || (eval g))
		| LESS(ex1, ex2) -> ((evalexpr ex1) < (evalexpr ex2))

