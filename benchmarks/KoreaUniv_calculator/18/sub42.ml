type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp ->
  match exp with
  INT(exp)->exp
  |ADD(a,b)->calculator(a)+calculator(b)
  |SUB(a,b)->calculator(a)-calculator(b)
  |MUL(a,b)->calculator(a)*calculator(b)
  |DIV(a,b)->calculator(a)/calculator(b)
  |SIGMA(a,b,c)->if calculator a<=calculator b then calculator c else calculator a+1;;
  