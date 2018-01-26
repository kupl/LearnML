(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

type env = (exp * int) list
let empty_env = []
let extend_env (x, v) e = (x, v)::e

let rec apply_env x e = match e with |[] -> raise(Failure "n") |(y, v)::tl -> if (x = y) then v else apply_env x tl

let rec eval: exp -> env -> int
= fun exp env -> match exp with | X -> apply_env X env | INT n -> n | ADD (e1, e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1, v2 with | n1, n2 -> (n1 + n2)) | SUB(e1, e2) -> let v1 = eval e1 env in let v2 = eval e2 env in(match v1, v2 with | n1, n2 -> (n1 - n2)) | MUL (e1, e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1, v2 with |n1, n2 -> (n1 * n2)) | DIV (e1, e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1, v2 with |n1, n2 -> (n1 / n2)) | SIGMA (e1, e2, e3) -> let v1 = eval e1 env in let v2 = eval e2 env in (if v1 > v2 then 0 else (eval e3 (extend_env (X, v1) env)) + eval (SIGMA(ADD(INT 1, e1) , e2, e3)) env)

let calculator : exp -> int
= fun e -> eval e empty_env
