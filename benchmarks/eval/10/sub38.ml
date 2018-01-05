type expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr | MULT of expr * expr | DIVIDE of expr * expr | MAX of expr list

exception DividedByZero
let rec eval e = match e with
		NUM i -> i
		| PLUS (e1, e2) -> (eval e1)+(eval e2)
		| MINUS (e1, e2) -> (eval e1)-(eval e2)
		| MULT (e1, e2) -> (eval e1)*(eval e2)
		| DIVIDE (e1, e2) -> let ee2 = (eval e2) in
					if ee2 = 0 then raise DividedByZero
					else (eval e1)/(eval e2)
		| MAX [] -> 0
		| MAX (h::[]) -> (eval h)
		| MAX (h::t) -> let maxt = (eval (MAX t)) in
				let eh = (eval h) in
				if (eh > maxt) then eh
				else maxt
