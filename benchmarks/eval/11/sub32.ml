(* 2009-13384 CHO Hyunik *)


type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list
exception DivByZero



let rec eval exp = 
	match exp with
	NUM a -> a
	| PLUS(a,b) -> (eval a) + (eval b)
	| MINUS(a,b) -> (eval a) - (eval b)
	| MULT(a,b) -> (eval a) * (eval b)
	| DIVIDE(a,b) -> 
		if (eval b)==0
		then raise DivByZero
		else (eval a) / (eval b) 
	| MAX [] -> 0
	| MAX (a::[]) -> (eval a)
	| MAX (a::b) -> max (eval a) (eval (MAX b))