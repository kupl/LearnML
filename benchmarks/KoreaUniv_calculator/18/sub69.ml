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
  | X -> 
  | INT int -> int
  | ADD (exp1, exp2) -> (calculator(exp1) + calculator(exp2))
  | SUB (exp1, exp2) -> (calculator(exp1) - calculator(exp2))
  | MUL (exp1, exp2) -> (calculator(exp1) * calculator(exp2))
  | DIV (exp1, exp2) -> (calculator(exp1) / calculator(exp2))
  | SIGMA (exp1, exp2, exp3) -> 
    