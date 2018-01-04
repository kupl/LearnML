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

let rec exp e = 
	match e with
		| NUM x -> x
		| PLUS (x, y) -> (exp x) + (exp y)
		| MINUS (x, y) -> (exp x) - (exp y)

let rec eval form = 
	match form with
		| TRUE -> true
		| FALSE -> false
		| NOT f -> not (eval f)
		| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
		| ORELSE (f1, f2) -> (eval f1) || (eval f2)
		| IMPLY (f1, f2) -> not ((eval f1) && (not (eval f2)))
		| LESS (e1, e2) -> if (exp e1) < (exp e2) then true
				else false
