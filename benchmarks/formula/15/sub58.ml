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
	| PLUS(x,y) -> x + y
	| MINUS(x,y) -> x - y

let rec eval form =
	match form with
	| TRUE -> true
	| FALSE -> false
	| ANDALSO(x,y) -> x && y
	| NOT(x) -> not x
	| ORELSE(x,y) -> x || y
	| IMPLY(x,y) -> x || (not y)
	| LESS(x,y) -> calc(x) < calc(y)
