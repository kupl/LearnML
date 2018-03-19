type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e -> match e with 
		| INT x -> x
		| ADD (a1, b1) -> (calculator a1) + (calculator b1)
		| SUB (a2, b2) -> (calculator a2) - (calculator b2)
		| MUL (a3, b3) -> (calculator a3) * (calculator b3)
    | DIV (a4, b4) -> (calculator a4) / (calculator b4)
		| SIGMA(a5, b5, c5) -> (calculator a5)
