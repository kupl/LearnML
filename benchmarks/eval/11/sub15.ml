
(* 2008-11720 Á¶°Ü¸® *)

type expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr
		| MULT of expr * expr
		| DIVIDE of expr * expr
		| MAX of expr list

let rec eval ex =
	match ex with
	NUM i -> i
	| PLUS (e1, e2) -> (eval e1)+(eval e2)
	| MINUS (e1, e2) -> (eval e1)-(eval e2)
	| MULT (e1, e2) -> (eval e1)*(eval e2)
	| DIVIDE (e1, e2) -> (eval e1)/(eval e2)
	| MAX exlist -> ( match exlist with
			[] -> 0
			| _ -> (List.fold_left max 0 (List.map eval exlist))
			)

