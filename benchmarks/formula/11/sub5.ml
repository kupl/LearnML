
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

let rec calexpr expr =
	match expr with
	|NUM a -> a
	|PLUS(a,b) -> calexpr(a) + calexpr(b)
	|MINUS(a,b) -> calexpr(a) - calexpr(b)

let rec eval formula = 
	match formula with
	|TRUE -> true
	|FALSE -> false
	|NOT f1 -> not(eval f1)
	|ANDALSO(f1, f2) -> (eval f1) && (eval f2)
	|ORELSE(f1, f2) -> (eval f1) || (eval f2)
	|IMPLY(f1, f2) -> not(eval f1) || (eval f2)
	|LESS(e1, e2) -> calexpr(e1) < calexpr(e2)
