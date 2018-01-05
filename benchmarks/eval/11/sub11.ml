type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

exception DivideByZero

	
let rec eval expr = 
	match expr with
	NUM(x) -> x
	| PLUS(x,y) -> eval(x) + eval(y)
	| MINUS(x,y) -> eval(x) - eval(y)
	| MULT(x,y) -> eval(x) * eval(y)
	| DIVIDE(x,y) -> if eval(y) = 0 then raise DivideByZero
		else eval(x) / eval(y)
	| MAX(x) -> (match x with
		[] -> 0
		| _ -> let a = min_int in List.fold_left findMax a x)

and findMax a expr =
	if eval(expr) > a then eval(expr)
	else a
	
