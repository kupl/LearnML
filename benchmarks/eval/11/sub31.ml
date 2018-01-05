type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval x =
	let rec getmax lst =
		match lst with
		[] -> 0
		| [x] -> (eval x)
		| [y;z] -> if ((eval y)>(eval z)) then (eval y) else (eval z)
		| h::t -> (getmax [h;(NUM (getmax t))])
	in
	match x with
	NUM y -> y
	| PLUS (y, z) -> (eval y)+(eval z)
	| MINUS (y, z) -> (eval y)-(eval z)
	| MULT (y, z) -> (eval y)*(eval z)
	| DIVIDE (y, z) -> (eval y)/(eval z)
	| MAX lst -> (getmax lst)
