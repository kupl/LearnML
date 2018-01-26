
(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let rec assign (x,n) =
match x with
SIGMA(a,b,c) -> assign (c,n)
| ADD(a,b) -> (assign (a,n)) + (assign (b,n))
| SUB(a,b) -> (assign (a,n)) - (assign (b,n))
| DIV(a,b) -> (assign (a,n)) / (assign (b,n))
| MUL(a,b) -> (assign (a,n)) * (assign (b,n))
| INT i -> i
| X -> n;;

let rec evalu x =
match x with
INT i-> i
| X -> raise(Failure("Error: Unassigned Variable"))
| ADD(a,b) -> (evalu a) + (evalu b)
| SUB(a,b) -> (evalu a) - (evalu b)
| MUL(a,b) -> (evalu a) * (evalu b)
| DIV(a,b) -> (evalu a) / (evalu b)
| SIGMA(a,b,c) -> if evalu a = evalu b then assign(c,evalu(a)) else assign(c,evalu(a))+evalu(SIGMA(INT(evalu(a)+1),b,c));;

let calculator : exp -> int
= fun e -> evalu e;;
