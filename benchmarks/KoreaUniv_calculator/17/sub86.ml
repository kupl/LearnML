(*problem 5*)
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
match e with
| INT n -> n
| ADD (a,b) -> calculator a + calculator b
| SUB (a,b) -> calculator a - calculator b
| MUL (a,b) -> calculator a * calculator b
| DIV (a,b) -> if calculator b = 0 then raise (Failure "divided by zero")   
  else calculator a / calculator b
| X -> raise (Failure "no value in the X") 
| SIGMA (a,b,exp) -> 
let rec eval exp env  = 
match (exp, env) with
| (INT n, _) -> n
| (ADD (a, b), env) -> eval a env + eval b env
| (SUB (a, b), env) -> eval a env - eval b env
| (MUL (a, b), env) -> eval a env * eval b env
| (DIV (a, b), env) -> if eval b env = 0 then raise (Failure "divided by zero")
                       else eval a env / eval b env
| (X, []) -> raise (Failure "no value in the X") 
| (X, hd::tl) -> hd 
| (SIGMA (a, b, ex), env) -> let s = eval a env in let e = eval b env in
    if s > e then raise (Failure "starting point is bigger than the ending point")
    else if s = e then (eval ex (s::env))
    else (eval ex (s::env)) + (eval (SIGMA (INT (s+1),INT e, ex)) env)
in
eval (SIGMA (a,b,exp)) []
