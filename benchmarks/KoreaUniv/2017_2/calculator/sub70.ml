(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> 
	match e with
	| ADD(a,b) -> calculator(a) + calculator(b)
	| SUB(a,b) -> calculator(a) - calculator(b)
	| MUL(a,b) -> calculator(a) * calculator(b)
	| DIV(a,b) -> calculator(a) / calculator(b)
	| SIGMA(a,b,s) ->
		let rec substitute : exp -> int -> exp
		= fun e n ->
			match e with
			| X -> INT(n)
			| ADD(a,b) -> ADD(substitute a n,substitute b n)
			| SUB(a,b) -> SUB(substitute a n,substitute b n)
			| MUL(a,b) -> MUL(substitute a n,substitute b n)
			| DIV(a,b) -> DIV(substitute a n,substitute b n)
			| SIGMA(a,b,c) -> INT(calculator(SIGMA(a,b,c)))
			| INT(a) -> INT(a)
		in
		if (calculator(a)==calculator(b)) then calculator(substitute s (calculator(a)))
		else calculator(SIGMA(INT(calculator(ADD(a,INT(1)))), b, s)) + calculator(substitute s (calculator(a)))
	| X -> 0
	| INT(a) -> a