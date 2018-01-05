exception DividedByZero

type expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr | MULT of expr * expr | DIVIDE of expr * expr | MAX of expr list

let rec eval e =
	match e with
		NUM n -> n
		| PLUS (e1,e2) -> (eval e1)+(eval e2)
		| MINUS (e1,e2) -> (eval e1)-(eval e2)
		| MULT (e1,e2) -> (eval e1)*(eval e2)
		| DIVIDE (e1,e2) -> if ((eval e2)=0) then raise DividedByZero else (eval e1)/(eval e2)
		| MAX [] -> 0
		| MAX (e1::e2) -> max (eval e1) (eval (MAX e2))