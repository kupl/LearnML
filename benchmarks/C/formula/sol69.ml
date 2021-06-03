
type formula = True
	     | False
	     | Not of formula
	     | AndAlso of formula * formula
	     | OrElse of formula * formula
	     | Imply of formula * formula
	     | Equal of exp * exp
and exp = Num of int
	 | Plus of exp * exp
	 | Minus of exp * exp


let rec eval form =

	let rec cal ex =
        	match ex with
                	| Num a -> a
               		| Plus(a,b) -> (cal a) + (cal b)
               	 	| Minus(a,b) -> (cal a) - (cal b)
	in

	match form with
		| True -> true
		| False -> false
		| Not f -> (match (eval f) with
				| true -> false
				| false -> true)
		| AndAlso(f1, f2) -> (match ((eval f1), (eval f2)) with
						| (true, true) -> true
						| (_, _) -> false )
		| OrElse(f1, f2) -> (match ((eval f1), (eval f2)) with
						| (false, false) -> false
						| (_, _) -> true )
		| Imply(f1, f2) -> (match ((eval f1), (eval f2)) with
						| (true, false) -> false
						| (_, _) -> true )
		| Equal(e1, e2) -> (if ((cal e1) = (cal e2)) then true
				   else false)
