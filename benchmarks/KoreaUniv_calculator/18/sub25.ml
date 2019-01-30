type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
  
  
  

  SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1));;

"let rec calculator : exp -> int
= fun exp -> 0
|SIGMA of exp*exp*exp = INT of int * INT of int * exp"


