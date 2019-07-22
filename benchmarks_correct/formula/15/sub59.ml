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

let rec calc ex =
	match ex with
	| NUM(i) -> i
	| PLUS(x,y) -> calc(x) + calc(y)
	| MINUS(x,y) -> calc(x) - calc(y)

let rec eval form =
	match form with
	| TRUE -> true
	| FALSE -> false
	| ANDALSO(x,y) -> eval(x) && eval(y)
	| NOT(x) -> not(eval(x))
	| ORELSE(x,y) -> eval(x) || eval(y)
	| IMPLY(x,y) -> (not(eval(x))) || eval(y)
	| LESS(x,y) -> calc(x) < calc(y)
