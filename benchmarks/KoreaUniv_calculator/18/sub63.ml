type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
let rec eval : exp->exp->int
 = fun exp x ->
 match exp with
  | X -> calculator x
  | INT n -> n
  | ADD (e1, e2) -> (eval e1 x) + (eval e2 x)
  | SUB (e1, e2) -> (eval e1 x) - (eval e2 x)
  | MUL (e1, e2) -> (eval e1 x) * (eval e2 x)
  | DIV (e1, e2) -> (eval e1 x) / (eval e2 x)
  | SIGMA(e1, e2, e3) -> if (eval e1 x = eval e2 x) then eval e3 (INT(eval e1 x)) else (eval e3 (INT(eval e1 x)))+(calculator (SIGMA(ADD(INT(eval e1 x), INT(1)), (INT(eval e2 x)), e3))) in
  match e with
    |INT(a) -> a
    |ADD(e1, e2) -> calculator e1 + calculator e2
    |SUB(e1, e2) -> calculator e1 - calculator e2
    |MUL(e1, e2) -> calculator e1 * calculator e2
    |DIV(e1, e2) -> calculator e1 / calculator e2
    |SIGMA(e1, e2, e3) -> if (calculator e1= calculator e2) then eval e3 e1 else (eval e3 e1)+(calculator (SIGMA(ADD(e1, INT 1), e2, e3) ) );;
