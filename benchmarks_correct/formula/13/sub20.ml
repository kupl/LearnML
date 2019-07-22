type formula = TRUE
	|FALSE
	|NOT of formula
	|ANDALSO of formula * formula
	|ORELSE of formula * formula
	|IMPLY of formula * formula
	|LESS of expr * expr
and expr = NUM of int
	|PLUS of expr * expr
	|MINUS of expr * expr

let rec evalexpr e =
	match e with
	|NUM i -> i
	|PLUS (e1,e2) -> (evalexpr e1) + (evalexpr e2)
	|MINUS (e1,e2) -> (evalexpr e1) - (evalexpr e2)

let rec eval f =
	match f with
	|TRUE -> true
	|FALSE -> false
	|NOT f -> not (eval f)
	|ANDALSO (f1,f2) -> if ((eval f1) = true) && ((eval f2) = true) then true
			     else false
	|ORELSE (f1,f2) -> if ((eval f1) = true) || ((eval f2) = true) then true
			   else false
	|IMPLY (f1,f2) -> if ((eval f1) = true) && ((eval f2) = false) then false
			  else true
	|LESS (e1,e2) -> if (evalexpr e1) < (evalexpr e2) then true
			 else false
