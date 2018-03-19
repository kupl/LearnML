type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec _calc : exp * exp -> exp
=fun (e1, e2) -> match e2 with
  X -> MUL(e1, X)
| INT i -> INT i
| ADD (a, b) -> ADD (_calc(e1, a), _calc(e1, b))
| SUB (a, b) -> SUB (_calc(e1, a), _calc(e1, b))
| MUL (a, b) -> MUL (_calc(e1, a), _calc(e1, b))
| DIV (a, b) -> DIV (_calc(e1, a), _calc(e2, b))

let rec calculator : exp -> int
=fun e -> match e with
  X -> 1
| INT i -> i
| ADD (a, b) -> calculator a + calculator b
| SUB (a, b) -> calculator a - calculator b
| MUL (a, b) -> calculator a * calculator b
| DIV (a, b) -> calculator a / calculator b
| SIGMA (a, b, c) ->	if calculator a > calculator b
							then 0
						else
							calculator (_calc (a, c)) + calculator (SIGMA ((INT (calculator (ADD (a, INT 1))), b, c)));;


						  
