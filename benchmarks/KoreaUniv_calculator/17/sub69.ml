(* problem 5*) 
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec calculator : exp -> int = fun e 
-> match e with
| INT a -> a
| ADD (a, b) -> (calculator a) + (calculator b)
| SUB (a, b) -> (calculator a) - (calculator b)
| MUL (a, b) -> (calculator a) * (calculator b)
| DIV (a, b) -> (calculator a) / (calculator b)
| SIGMA (a, b, c) -> if (calculator a)>(calculator b) then 0 
   else eval(c,a) + calculator (SIGMA ( ADD( a, INT 1),  b, c))
and eval (f, x) = match f with
| X -> calculator x
| INT n -> n
| ADD(a,b) -> eval(a, x) + eval (b,x)
| SUB(a,b) -> eval(a, x) - eval (b,x)
| MUL(a,b) -> eval(a, x) * eval (b,x)
| DIV(a,b) -> eval(a, x) / eval (b,x)
