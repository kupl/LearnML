type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

let rec eval ex =
	match ex with
		| NUM(n) -> n
		| PLUS(a,b) -> (eval a) + (eval b)
		| MINUS(a,b) -> (eval a) - (eval b)
		| MULT(a,b) -> (eval a) * (eval b)
		| DIVIDE(a,b) -> (eval a) / (eval b)
		| MAX([]) -> 0
		| MAX([s]) -> (eval s)
		| MAX(s::t) -> max (eval s) (eval (MAX(t)))
