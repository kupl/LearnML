type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

exception Error of string

let max(a,b) = 
	if(a>b) then a
	else b 

let rec array_max(l) = 
	match l with
		| hd::ta -> max(eval(hd), array_max(ta))
		| [] -> 0

and eval(expr) = 
	match expr with
		| NUM(a) -> a 
		| PLUS(a,b) -> eval(a) + eval(b)
		| MINUS(a,b) -> eval(a) - eval(b)
		| MULT(a,b) -> eval(a) * eval(b)
		| DIVIDE(a,b) -> (
				if(eval(b)==0) then (raise (Error("Divided by Zero")))
				else eval(a) / eval(b)
			)
		| MAX(l) -> array_max(l)
