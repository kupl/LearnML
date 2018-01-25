type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let rec calculator : exp -> int
=fun e ->
match e with
| X -> raise (Failure "Type Error")
| INT n -> n
| ADD (x,y) -> (calculator x) + (calculator y)
| SUB (x,y) -> (calculator x) - (calculator y)
| MUL (x,y) -> (calculator x) * (calculator y)
| DIV (x,y) -> (calculator x) / (calculator y)
| SIGMA (x,y,z) -> if calculator x > calculator y  then 0 else cal (z,x) + calculator (SIGMA (ADD(x, INT 1),y,z))
and cal (z,x) =
match z with
| X -> calculator x
| INT n -> n
| ADD (a,b) -> cal (a,x) + cal(b,x)
| SUB (a,b) -> cal (a,x) - cal(b,x)
| MUL (a,b) -> cal (a,x) * cal(b,x)
| DIV (a,b) -> cal (a,x) / cal(b,x)
| SIGMA (a,b,c) -> calculator (SIGMA (a,b,c))



