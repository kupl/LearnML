type expr = NUM of int
		  | PLUS of expr * expr
		  | MINUS of expr * expr
		  | MULT of expr * expr
		  | DIVIDE of expr * expr
		  | MAX of expr list

let rec eval expr =
	let rec max lst m = 
		match lst with
		  [] -> m
		| h::t -> if h > m then max t h else max t m
	in
	match expr with
	  NUM n -> n
	| PLUS (e1, e2) -> (eval e1) + (eval e2)
	| MINUS (e1, e2) -> (eval e1) - (eval e2)
	| MULT (e1, e2) -> (eval e1) * (eval e2)
	| DIVIDE (e1, e2) -> (eval e1) / (eval e2)
	| MAX [] -> 0
	| MAX (head::tail) -> max (List.map eval tail) (eval head)
