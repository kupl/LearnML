type exp =
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let calculator : exp -> int
=fun e -> match e with
|INT x -> x
|ADD(INT x,INT y) -> x+y
|SUB(INT x,INT y) -> x-y
|MUL(INT x,INT y) -> x*y
|DIV(INT x,INT y) -> x/y
|SIGMA(INT x, INT y,e3) -> if x=y then (calculator e3)
 else (calculator e3) + (calculator (SIGMA(INT x,INT (y-1),e3)));;