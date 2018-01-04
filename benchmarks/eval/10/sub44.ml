type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

exception EmptyList

let rec eval targetExp = match targetExp
	with NUM integer -> integer
	| PLUS (subExp1, subExp2) -> (eval subExp1) + (eval subExp2)
	| MINUS (subExp1, subExp2) -> (eval subExp1) - (eval subExp2)
	| MULT (subExp1, subExp2) -> (eval subExp1) * (eval subExp2)
	| DIVIDE (subExp1, subExp2) -> (eval subExp1) / (eval subExp2)
	| MAX expList -> getMax expList
and getMax expList = match expList
	with [] -> 0
	| [exp] -> eval exp
	| head::rest -> getBigger (eval head) (getMax rest)
and getBigger val1 val2 =
	if val1 > val2 then val1 else val2
