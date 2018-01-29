(* problem 5*)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun e -> match e with INT b->b | ADD (c,d) -> calculator c + calculator d | SUB (c,d) -> 
calculator c - calculator d | MUL (c,d) -> calculator c * calculator d | DIV (c,d) -> calculator c / calculator d
