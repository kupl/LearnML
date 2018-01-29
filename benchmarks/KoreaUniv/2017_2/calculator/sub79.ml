(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec xtoint : exp -> int -> int
= fun exp i ->
match exp with
| INT a -> a
| X -> i
| ADD (a,b) -> xtoint a i + xtoint b i
| SUB (a,b) -> xtoint a i - xtoint b i
| MUL (a,b) -> xtoint a i * xtoint b i
| DIV (a,b) -> xtoint a i / xtoint b i

let rec calculator : exp -> int
= fun e ->
match e with
| INT a -> a
| ADD (a,b) -> calculator a + calculator b
| SUB (a,b) -> calculator a - calculator b
| MUL (a,b) -> calculator a * calculator b
| DIV (a,b) -> calculator a / calculator b
| SIGMA (first, last, expr) -> 
if (calculator first) > (calculator last) then 0
else (xtoint expr (calculator first)) + (calculator (SIGMA((ADD(first,INT 1)), last, expr)))

