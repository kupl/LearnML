(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e ->
          let rec eval exp env =  match exp with
                             | X -> env
                             | INT (n) ->  n
                             | ADD (a1, b1) -> let v1 = eval a1 env in let v2 = eval b1 env in v1 + v2
                             | SUB (a1, b1) -> let v1 = eval a1 env in let v2 = eval b1 env in v1 - v2
                             | MUL (a1, b1) -> let v1 = eval a1 env in let v2 = eval b1 env in v1 * v2
                             | DIV (a1, b1) -> let v1 = eval a1 env in let v2 = eval b1 env in v1 / v2
                             | SIGMA(a1,a2,b1) -> let v1 = eval a1 env in let v2 = eval a2 env in let rec func a = if a = v2 then  eval b1 a else eval b1 a + func (a+1) in (func v1)
                             in eval e 0