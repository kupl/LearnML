type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

exception DivideByZero

let rec eval
=fun e ->
	match e with 
		X -> raise InvalidArgument
		| INT i -> i
		| ADD (e1, e2) -> 
			let r1 = eval e1 in
			let r2 = eval e2 in
				r1 + r2
		| SUB (e1, e2) -> 
			let r1 = eval e1 in
			let r2 = eval e2 in
				r1 - r2
		| MUL (e1, e2) ->  
			let r1 = eval e1 in
			let r2 = eval e2 in
				r1 * r2
		| DIV (e1, e2) -> 
			let r1 = eval e1 in
			let r2 = eval e2 in
				if r2 = 0 then raise DivideByZero else r1 / r2
		| SIGMA (sc, tc, e) -> 
			let sc' = eval sc in
			let tc' = eval tc in
			if sc' > tc' then 0 
			else 
			(
				eval (SIGMA (INT (sc'+1), INT tc', e)) + (sigma e sc')
			)

and sigma
=fun e i ->
	match e with
		X -> i
		| INT n -> n
		| ADD (e1, e2) -> eval (ADD (INT (eval (SIGMA (INT i, INT i, e1))) ,INT (eval (SIGMA (INT i, INT i, e2)))))
		| SUB (e1, e2) -> eval (SUB (INT (eval (SIGMA (INT i, INT i, e1))) ,INT (eval (SIGMA (INT i, INT i, e2)))))
		| MUL (e1, e2) -> eval (MUL (INT (eval (SIGMA (INT i, INT i, e1))) ,INT (eval (SIGMA (INT i, INT i, e2)))))
		| DIV (e1, e2) -> eval (DIV (INT (eval (SIGMA (INT i, INT i, e1))) ,INT (eval (SIGMA (INT i, INT i, e2)))))
		| SIGMA (sc, tc, e') -> eval (SIGMA (sc, tc, INT (eval (SIGMA (INT i, INT i, e')))))

let calculator : exp -> int
=fun e -> eval e
