type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


    
let rec calculator : exp -> int
= fun exp ->  (*TODO*)
  match exp with 
    |INT exp -> exp
    |ADD(e1,e2)->calculator (e1)+calculator (e2)
    |SUB(e1,e2)->calculator (e1)-calculator (e2)
    |MUL(e1,e2)->calculator (e1)*calculator (e2)
    |DIV(e1,e2)->calculator (e1)/calculator (e2)
    |SIGMA(e1,e2,e3)-> if calculator (e1) <= calculator (e2)
    then calculator (e3) else calculator (e1) + 1;;
let rec sigma : exp -> exp 
= fun exp->
  match exp with 
    |ADD(e1,e2)->sigma(calculator e1+calculator e2)
    |SUB(e1,e2)->sigma(calculator e1-calculator e2)
    |MUL(e1,e2)->sigma(calculator e1*calculator e2)
    |DIV(e1,e2)->sigma(calculator e1/calculator e2);;
