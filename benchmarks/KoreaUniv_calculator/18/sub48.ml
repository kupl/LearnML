type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> match exp with
  |INT(x) -> x
  |ADD(x,y) -> calculator(x) + calculator(y)
  |SUB(x,y) -> calculator(x) - calculator(y)
  |MUL(x,y) -> calculator(x) * calculator(y)
  |DIV(x,y) -> calculator(x) / calculator(y)
  |SIGMA(x,y) -> 
