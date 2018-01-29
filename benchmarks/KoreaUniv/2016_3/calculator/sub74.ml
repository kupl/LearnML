
type exp =
| X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun exp ->
   match exp with
   | X -> 1
   | INT n -> n
   | ADD (e1, e2) -> calculator e1 + calculator e2
   | SUB (e1, e2) -> calculator e1 - calculator e2
   | MUL (e1, e2) -> calculator e1 * calculator e2
   | DIV (e1, e2) -> calculator e1 / calculator e2
   | SIGMA (e1, e2, e3) ->
   if (calculator e1)=(calculator e2) then
   calculator e3
   else let i=calculator e1 in
   let k= calculator e2 in
   calculator e3 + calculator(SIGMA(INT (i+1),INT k,e3))
