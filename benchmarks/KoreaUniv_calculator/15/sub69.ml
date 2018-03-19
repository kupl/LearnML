type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let rec calculator : exp -> int
=fun e -> match e with
X -> raise(Failure "error")
| INT x -> x
| ADD(x,y) -> (calculator x) + (calculator y)
| SUB(x,y) -> (calculator x) - (calculator y)
| MUL(x,y) -> (calculator x) * (calculator y)
| DIV(x,y) -> (calculator x) / (calculator y)
| SIGMA(x,y,z) -> (let rec cal = fun e -> match e with
X -> cal y
| INT a -> a
| ADD(a,b) -> (cal a) + (cal b)
| SUB(a,b) -> (cal a) - (cal b)
| MUL(a,b) -> (cal a) * (cal b)
| DIV(a,b) -> (cal a) / (cal b)
| SIGMA(x,y,z) -> raise(Failure "error") in
if (calculator x) < (calculator y)
then (cal z) + calculator(SIGMA(x,(INT((calculator y)-1)),z))
else cal z);;
