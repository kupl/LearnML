type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun ee -> 
  let rec eval = fun e i ->
    match e with
      X -> i
      | INT(s) -> s
      | ADD(a,b) -> (eval a i)+(eval b i)
      | SUB(a,b) -> (eval a i)-(eval b i)
      | MUL(a,b) -> (eval a i)*(eval b i)
      | DIV(a,b) -> (eval a i)/(eval b i)
      | SIGMA(INT(a),INT(b),c) -> if a>b then 0 else (eval c a)+(eval (SIGMA( INT(a+1),INT(b),c)) a)
      | SIGMA(a,b,c)-> 0
  in
  eval ee 1;;
  