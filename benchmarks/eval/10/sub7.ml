type expr = NUM of int
	  | PLUS of expr * expr
	  | MINUS of expr * expr
	  | MULT of expr * expr
	  | DIVIDE of expr * expr
	  | MAX of expr list

let rec eval expr =

let rec maxe elist =
	if List.length elist = 0 then 0
	else if List.length elist = 1 then eval(List.hd elist)
	else if eval(List.hd elist) > maxe (List.tl elist)
	then eval(List.hd elist)
	else maxe (List.tl elist) in
	
	match expr with
	  NUM(no) -> no
	| PLUS(e1, e2) -> (eval e1) + (eval e2)
	| MINUS(e1, e2) -> (eval e1) - (eval e2)
	| MULT(e1, e2) -> (eval e1) * (eval e2)
	| DIVIDE(e1, e2) -> (eval e1) / (eval e2)
	| MAX(elist) -> maxe(elist)
