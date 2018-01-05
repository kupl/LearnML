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
let rec valueofexpr expr =
	match expr with
	| NUM (num) -> num
	| PLUS (e1, e2) -> valueofexpr(e1) + valueofexpr(e2)
	| MINUS (e1, e2) -> valueofexpr(e1) - valueofexpr(e2)
let rec eval form = 
	match form with
	| TRUE -> true
	| FALSE -> false
	| NOT (formula) -> not(eval(formula))
	| ANDALSO (f1, f2) -> eval(f1) && eval(f2)
	| ORELSE (f1, f2) -> eval(f1) || eval(f2)
	| IMPLY (f1, f2) -> not(eval(f1)) || eval(f2)
	| LESS (e1, e2) -> if valueofexpr(e1) < valueofexpr(e2) then true else false
