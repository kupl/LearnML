type expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr | MULT of expr * expr | DIVIDE of expr * expr | MAX of expr list

let rec eval _expr =
	match _expr with
	NUM(_val) -> _val
	| PLUS(_expr1, _expr2) -> eval(_expr1) + eval(_expr2)
	| MINUS(_expr1, _expr2) -> eval(_expr1) - eval(_expr2)
	| MULT(_expr1, _expr2) -> eval(_expr1) * eval(_expr2)
	| DIVIDE(_expr1, _expr2) -> eval(_expr1) / eval(_expr2)
	| MAX(expr_list) -> findMax(expr_list, 0, 0)



and findMax (expr_list, flag, max) =
	match expr_list with
	[] -> max
	| h::t -> (
			if flag=0 then findMax(t, 1, eval(h))
			else if eval(h)>max then findMax(t, flag, eval(h))
			else findMax(t, flag, max))
			

