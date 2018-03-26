(*problem5*)
exception Problem
type exp = 
	| X 
	| INT of int 
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun e -> 
match e with
	| X -> raise Problem
	| INT n -> n
	| ADD (e1,e2) -> calculator e1 + calculator e2
	| SUB (e1,e2) -> calculator e1 - calculator e2
	| MUL (e1,e2) -> calculator e1 * calculator e2
	| DIV (e1,e2) -> calculator e1 / calculator e2
	| SIGMA (e1,e2,e3) -> 
	let p,q = calculator e1, calculator e2 in
		if (p < q) 
		then (sigma_sub e3 p + calculator (SIGMA (INT(p +1), INT q, e3)))
		else if p = q then sigma_sub e3 p 
		else raise Problem

and sigma_sub : exp -> int -> int
= fun e n ->
match e with
	| X -> n
	| INT k -> k
	| ADD (e1,e2) -> sigma_sub e1 n + sigma_sub e2 n
	| SUB (e1,e2) -> sigma_sub e1 n - sigma_sub e2 n
	| MUL (e1,e2) -> sigma_sub e1 n * sigma_sub e2 n
	| DIV (e1,e2) -> sigma_sub e1 n / sigma_sub e2 n
	| SIGMA (e1,e2,e3) -> calculator e
