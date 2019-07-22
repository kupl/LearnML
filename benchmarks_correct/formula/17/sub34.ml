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

let rec expr_to_int : expr -> int = fun expr ->
	match expr with
	| NUM x -> x
	| PLUS (e1, e2) -> expr_to_int e1 + expr_to_int e2
	| MINUS (e1, e2) -> expr_to_int e1 - expr_to_int e2

let rec eval : formula -> bool = fun formula ->
	match formula with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> if eval f = true then false
			   else true
	| ANDALSO (f1, f2) -> if ((eval f1 = true) && (eval f2 = true)) then true
						  else false
	| ORELSE (f1, f2) -> if ((eval f1 = false) && (eval f2 = false)) then false
						 else true
	| IMPLY (f1, f2) -> if eval f1 = false then true
						else if eval f2 = true then true
						else false
	| LESS (e1, e2) -> if expr_to_int e1 < expr_to_int e2 then true
					   else false

