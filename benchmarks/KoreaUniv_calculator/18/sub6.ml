type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
let rec calculator : exp -> int
= fun exp -> (*0*)
match exp with
  |INT(n)-> n
  |ADD(exp1, exp2)-> calculator exp1 + calculator exp2
  |SUB(exp1, exp2)-> calculator exp1 - calculator exp2
  |MUL(exp1, exp2)-> calculator exp1 * calculator exp2
  |DIV(exp1, exp2)-> calculator exp1 / calculator exp2
  
  |SIGMA(INT(n1), INT(n2), exp3)-> 
      if exp3 = INT(n) then (n2-n1+1)*n 
      else sigma makexfun exp3 n1 n2
  |SIGMA(INT(n1), exp2, exp3)-> calculator(SIGMA(INT(n1), calculator(exp2), exp3))
  |SIGMA(exp1, exp2, exp3)-> calculator(SIGMA(calculator(exp1), calculator(exp2), exp3))
  ;;
  