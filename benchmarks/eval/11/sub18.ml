(*
2008-12155
±èÂùÈ£
*)

type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

let rec eval form =
	let getmax = function
		[] -> 0
		| a::b -> List.fold_left max a b
	in

	match form with
	| NUM n -> n
	| PLUS(x, y) -> (eval x) + (eval y)
	| MINUS(x, y) -> (eval x) - (eval y)
	| MULT(x, y) -> (eval x) * (eval y)
	| DIVIDE(x, y) -> (eval x) / (eval y)
	| MAX l -> getmax (List.map eval l)