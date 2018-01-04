exception DivideByZero of string
type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval exp = 
	match exp with
	NUM(a) -> a
	| PLUS(exp1, exp2) -> eval exp1 + eval exp2
	| MINUS(exp1, exp2) -> eval exp1 - eval exp2
	| MULT(exp1, exp2) -> eval exp1 * eval exp2
	| DIVIDE(exp1, exp2) -> if (eval exp2 == 0) then raise (DivideByZero "can't divide by zero") else eval exp1 / eval exp2
	| MAX([]) -> 0
	| MAX(a::[]) -> eval a
	| MAX(a::b::[]) -> if (eval a >= eval b) then eval a else eval b
	| MAX(a::b::li) -> if (eval a >= eval b) then eval (MAX(a::li)) else eval (MAX(b::li));;


