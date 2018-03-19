type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

type opt = O of exp
				 | N

let rec eval : exp -> opt -> int
= fun e o-> match e with
| X -> (match o with
		|O x -> eval x N 
		|N -> raise (Failure "Cannot Identify"))
| INT n -> n
| ADD (e1,e2) -> (eval e1 o) + (eval e2 o)
| SUB (e1,e2) -> (eval e1 o) - (eval e2 o)
| MUL (e1,e2) -> (eval e1 o) * (eval e2 o)
| DIV (e1,e2) -> (eval e1 o) / (eval e2 o)
| SIGMA (e1, e2, e3) ->  if (eval e1 o) <= (eval e2 o)
	then
		let o = O e1
		in (eval e3 o) + (eval (SIGMA (ADD (e1, INT 1), e2, e3)) o)
	else 0

let calculator : exp -> int
=fun e -> eval e N
