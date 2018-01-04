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

let imply a b = 
	if a && not b 
		then false
	else
		true

let rec eval form = 
	match form with
	TRUE -> true
	|FALSE -> false
	|NOT f -> not (eval f)
	|ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	|ORELSE (f1, f2) -> (eval f1) || (eval f2)
	|IMPLY (f1, f2) -> imply (eval f1) (eval f2)
	|LESS (e1, e2) -> 
		if (exp e1) < (exp e2)
			then true
		else
			false
	and exp e =
		match e with
		|NUM n -> n
		|PLUS (e1, e2) -> (exp e1) + (exp e2)
		|MINUS (e1, e2) -> (exp e1) - (exp e2)
