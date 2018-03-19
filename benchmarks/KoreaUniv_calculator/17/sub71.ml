(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
;;

let x=ref 0;;
let rec calculator : exp -> int
= fun e ->
match e with
| X -> !x
| INT n -> n
| ADD (a,b) -> (calculator a) + (calculator b)
| SUB (a,b) -> (calculator a) - (calculator b)
| MUL (a,b) -> (calculator a) * (calculator b)
| DIV (a,b) -> (calculator a) / (calculator b)
| SIGMA (a,b,f) -> if ((calculator a)=(calculator b)) then (x:=(calculator a); (calculator f))
                   else (x:=(calculator a); (calculator f)) + (calculator (SIGMA (INT ((calculator a)+1),b,f)));;
