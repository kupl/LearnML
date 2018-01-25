type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | ISZERO of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

type value = Int of int | Bool of bool 
           | Procedure of var * exp * env 
           | RecProcedure of var * var * exp * env
and env = var -> value

let empty_env = fun _ -> raise (Failure "Environment is empty")
let extend_env (x,v) e = fun y -> if x = y then v else (e y)
let apply_env e x = e x
let create_env (f, x, e1, e) = extend_env (f, RecProcedure(f, x, e1, e)) e

let rec eval_bop: (int->int->int) -> program -> program -> env -> value
= fun op e1 e2 env ->
let v1 = eval e1 env in
let v2 = eval e2 env in
(match v1, v2 with
Int n1, Int n2 -> Int (op n1 n2)
|_-> raise (Failure "Type error"))

and eval: program -> env -> value
= fun exp env ->
let func = [] in
match exp with
CONST n -> Int n
|VAR x -> apply_env env x
|ADD (e1, e2) -> eval_bop (+) e1 e2 env
|SUB (e1, e2) -> eval_bop (-) e1 e2 env
|ISZERO e -> (let v = eval e env in
	match v with
	Int n -> if n = 0 then Bool true else Bool false
	|_ -> raise (Failure "Type error in ISZERO"))
|IF(e1, e2, e3) ->	(match eval e1 env with
	Bool true -> eval e2 env
	|Bool false -> eval e3 env
	|_-> raise (Failure "Type error in IF"))
|LET (x, e1, e2) -> let v1 = eval e1 env in eval e2 (extend_env (x, v1) env)
|LETREC (f, x, e1, e2) -> eval e2 (create_env (f, x, e1, env))
|PROC (x, e1) -> Procedure (x, e1, env) 
|CALL (e1, e2) -> let v2 = eval e2 env in
	(match (eval e1 env) with
	Procedure (x, e, envr) -> eval e (extend_env (x, v2) envr)
	|RecProcedure (f, x, e, envr)-> let envr1 = extend_env (x, v2) envr in
		eval e (create_env (f, x, e, envr1))
	|_-> raise (Failure "error in call"))

let run : program -> value
=fun pgm -> eval pgm empty_env
