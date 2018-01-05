type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

exception DividedByZero
exception NoEl

let rec eval exp =
	match exp with
		NUM i -> i
		| PLUS (e1, e2) -> (eval e1) + (eval e2)
		| MINUS (e1, e2) -> (eval e1) - (eval e2)
		| MULT (e1, e2) -> (eval e1) * (eval e2)
		| DIVIDE (e1, e2) -> 
			(if (eval e2) = 0 then raise DividedByZero else (eval e1) / (eval e2))
		| MAX (e1::tl) ->
			(match tl with
				e2::tl2 -> 
					(if (eval e1) > (eval e2) then eval (MAX (e1::tl2))
					 else eval (MAX tl)
					)
				| [] -> (eval e1)
			)
		| MAX [] -> raise NoEl;;
