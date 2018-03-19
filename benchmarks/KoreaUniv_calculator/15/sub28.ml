type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator_x
=fun e x -> match e with
| X ->
	(match x with
	| None -> raise (Failure "error")
	| Some s -> s)
| INT i -> i
| ADD(e1,e2) -> (calculator_x e1 x)+(calculator_x e2 x)
| SUB(e1,e2) -> (calculator_x e1 x)-(calculator_x e2 x)
| MUL(e1,e2) -> (calculator_x e1 x)*(calculator_x e2 x)
| DIV(e1,e2) -> (calculator_x e1 x)/(calculator_x e2 x)
| SIGMA(e1,e2,e3) ->
	let a = calculator_x e2 x in
	let rec sigma b f =
		if b > a then f
		else (let v = calculator_x e3 (Some b) in
			sigma (b+1) (v+f)) in
		sigma (calculator_x e1 x) 0 
let calculator : exp -> int
=fun e -> calculator_x e None