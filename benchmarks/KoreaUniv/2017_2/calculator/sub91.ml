(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
match e with
| INT n -> n
| ADD (e1, e2) -> (calculator e1)+(calculator e2)
| SUB (e1, e2) -> (calculator e1)-(calculator e2)
| MUL (e1, e2) -> (calculator e1)*(calculator e2)
| DIV (e1, e2) -> (calculator e1)/(calculator e2)