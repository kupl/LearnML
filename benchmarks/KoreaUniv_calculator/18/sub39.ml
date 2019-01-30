type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> let rec calc exp (var:int) =
  match exp with
  X-> var
  |MUL(exp1,exp2)-> (calc exp1 var) * (calc exp2 var)
  |ADD(exp1,exp2)-> (calc exp1 var) + (calc exp2 var)
  |SUB(exp1,exp2)-> (calc exp1 var) - (calc exp2 var)
  |DIV(exp1,exp2)-> (calc exp1 var) / (calc exp2 var)
  |INT n -> n
  |SIGMA(exp1, exp2, exp3)-> if (calc exp1 var) <= (calc exp2 var) 
  then (calc exp3 (calc exp1 var)) + (calc (SIGMA (ADD(exp1, INT 1), exp2, exp3)) var)
  else 0
  in calc exp 0;;
  
  