type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

exception DividedByZero
exception EmptyList
let rec eval e = match e with
	NUM a -> a
	| PLUS (e1, e2) -> (eval e1) + (eval e2)
	| MINUS (e1, e2) -> (eval e1) - (eval e2)
	| MULT (e1, e2) -> (eval e1) * (eval e2)
	| DIVIDE (e1, e2) -> let ee2 = eval e2 in
		if ee2 == 0 then raise DividedByZero 
		else (eval e1) / ee2
	| MAX (e1::[]) -> eval e1
	| MAX (e1::e2::t) -> 
	if (eval e1) > (eval e2) 
		then eval (MAX (e1::t))
		else eval (MAX (e2::t))
	| MAX [] -> raise EmptyList
