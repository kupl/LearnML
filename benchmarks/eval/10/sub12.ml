type expr =	NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr
		| MULT of expr * expr
		| DIVIDE of expr * expr
		| MAX of expr list

exception DividedByZero

let rec eval ex = 
	match ex with
	NUM n -> n
	|PLUS (e1, e2) -> (eval e1) + (eval e2)
	|MINUS (e1, e2) -> (eval e1) - (eval e2)
	|MULT (e1, e2) -> (eval e1) * (eval e2)
	|DIVIDE (e1, e2) ->
		let b = (eval e2) in
			if b = 0
				then raise DividedByZero
			else
				(eval e1) / b
	|MAX lst -> max_of_list (exprlist_to_intlist lst)

	and exprlist_to_intlist lst = 
		match lst with
		[] -> []
		| h::t -> (eval h)::(exprlist_to_intlist t)

	and max_of_list lst = 
		match lst with
		[] -> 0
		| h::[] -> h
		| h::t -> max h (max_of_list t)


