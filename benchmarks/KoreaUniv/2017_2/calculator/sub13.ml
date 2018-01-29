(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec eval_e: exp -> int -> int
= fun e env ->
	match e with
	| X -> env
	| INT i -> i
	| ADD (p,q) -> eval_e p env + eval_e q env
	| SUB (p,q) -> eval_e p env - eval_e q env
	| MUL (p,q) -> (eval_e p env)*(eval_e q env)
	| DIV (p,q) -> (eval_e p env)/(eval_e q env)
	| SIGMA (p,q,r) ->
		let a = eval_e p env in
		let b = eval_e q env in
		if a = b then eval_e r a
		else (eval_e r a) + (eval_e (SIGMA (INT (a+1), INT b, r)) env)

let rec calculator : exp -> int
= fun e -> eval_e e 1