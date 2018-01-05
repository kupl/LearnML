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

let rec cal expr = 
	match expr with
	| (NUM n) -> n
	| (PLUS (e1, e2)) -> (cal e1) + (cal e2)
	| (MINUS (e1, e2)) -> (cal e1) - (cal e2)

let rec eval formula = 
	match formula with
	| TRUE -> true
	| FALSE -> false
	| (NOT f) -> not(eval f)
	| (ANDALSO (f1, f2)) -> (eval f1) && (eval f2)
	| (ORELSE (f1, f2)) -> (eval f1) || (eval f2)
	| (IMPLY (f1, f2)) -> (match ((eval f1), (eval f2)) with
						| (true, false) -> false
						| _ -> true)
	| (LESS (e1, e2)) -> if (cal e1) < (cal e2) then true else false
