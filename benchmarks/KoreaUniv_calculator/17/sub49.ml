(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec eval : exp -> int -> int =
  fun ex env ->
  match ex with
  | X -> env
  | INT n -> n
  | ADD (a,b) -> (eval a env)+(eval b env)
  | SUB (a,b) -> (eval a env)-(eval b env)
  | MUL (a,b) -> (eval a env)*(eval b env)
  | DIV (a,b) -> (eval a env)/(eval b env)
  | SIGMA (n,m,e) -> if (eval n env) > (eval m env) then 0 else (eval e (eval n env)) + (eval (SIGMA ((INT((eval n env)+1)),m,e)) env);;

let calculator : exp -> int =
  fun ex -> eval ex 0;;
