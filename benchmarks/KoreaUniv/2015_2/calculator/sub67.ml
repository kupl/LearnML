
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let rec calculator : exp -> int
=fun e -> match e with
X -> raise (Failure "undefined X")
|INT i -> i
|ADD (e1, e2) -> (calculator e1) + (calculator e2)
|SUB (e1, e2) -> (calculator e1) - (calculator e2)
|MUL (e1, e2) -> (calculator e1) * (calculator e2)
|DIV (e1, e2) -> (calculator e1) / (calculator e2)
|SIGMA (e1, e2, e3) -> if e1 <= e2 then
	let rec cal g3 g1 = match g3 with
		X -> calculator g1
		|INT i -> i
		|ADD (e1, e2) -> (cal e1 g1) + (cal e2 g1)
		|SUB (e1, e2) -> (cal e1 g1) - (cal e2 g1)
		|MUL (e1, e2) -> (cal e1 g1) * (cal e2 g1)
		|DIV (e1, e2) -> (cal e1 g1) / (cal e2 g1)
		|SIGMA (e1, e2, e3) -> calculator (SIGMA (e1, e2, e3)) in
	(cal e3 e1) + calculator (SIGMA (INT ((calculator e1) + 1), e2, e3)) else
	0;;


