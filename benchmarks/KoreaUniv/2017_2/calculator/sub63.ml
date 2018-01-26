(*problem 5*)

type exp = X | INT of int | ADD of exp * exp | SUB of exp * exp |
MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp

let rec  calculator : exp -> int = fun e ->
let extend_env v e = v::e in
let rec lookup_env e =
match e with
|[] -> raise (Failure (" "))
|v::tl -> v
in
let rec eval : exp -> int list -> int = fun exp env ->
match exp with
|INT n -> n
|ADD (n1,n2) -> eval n1 env + eval n2 env
|SUB (n1,n2) -> eval n1 env - eval n2 env
|MUL (n1,n2) -> eval n1 env * eval n2 env
|DIV (n1,n2) -> eval n1 env / eval n2 env
|X -> lookup_env env
|SIGMA (n1,n2,n3) -> let v1 = eval n1 env in let env' = extend_env v1 env in
if(eval n1 env < eval n2 env)
then (eval n3 env') + (eval (SIGMA ((INT (v1+1)), n2,n3)) env)
else eval n3 env'
 in eval e [] ;;