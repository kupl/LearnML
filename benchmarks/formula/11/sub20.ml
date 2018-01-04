(* 2009-11674 ±è¿øÁø HW1-5 *)

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


let rec eval form =

	let rec cal ex =
        	match ex with
                	| NUM a -> a
               		| PLUS(a,b) -> (cal a) + (cal b)
               	 	| MINUS(a,b) -> (cal a) - (cal b)
	in

	match form with
		| TRUE -> true
		| FALSE -> false
		| NOT f -> (match (eval f) with
				| true -> false
				| false -> true)
		| ANDALSO(f1, f2) -> (match ((eval f1), (eval f2)) with
						| (true, true) -> true
						| (_, _) -> false )
		| ORELSE(f1, f2) -> (match ((eval f1), (eval f2)) with
						| (false, false) -> false
						| (_, _) -> true )
		| IMPLY(f1, f2) -> (match ((eval f1), (eval f2)) with
						| (true, false) -> false
						| (_, _) -> true )
		| LESS(e1, e2) -> (if ((cal e1) < (cal e2)) then true
				   else false)
