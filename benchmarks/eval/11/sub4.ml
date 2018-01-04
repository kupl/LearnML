(* 200511843 LEE JONGHO *)


type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

let rec eval e =
	let rec findmax l =
		match l with
		[] -> 0
		| h::t -> if (eval h)>(findmax t) then (eval h)
			else (findmax t) 
	in
	match e with
	NUM i -> i
	| PLUS (e1, e2) -> (eval e1)+(eval e2)
	| MINUS (e1, e2) -> (eval e1)-(eval e2)
	| MULT (e1, e2) -> (eval e1)*(eval e2)
	| DIVIDE (e1, e2) -> (eval e1)/(eval e2)
	| MAX l -> findmax l 