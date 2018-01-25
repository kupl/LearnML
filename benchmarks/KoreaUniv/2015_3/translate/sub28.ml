  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string
  and env = var list

  type nl_program = nl_exp
  and nl_exp = 
    | NL_CONST of int
    | NL_VAR of int
    | NL_ADD of nl_exp * nl_exp
    | NL_SUB of nl_exp * nl_exp
    | NL_ISZERO of nl_exp
    | NL_IF of nl_exp * nl_exp * nl_exp
    | NL_LET of nl_exp * nl_exp
    | NL_PROC of nl_exp 
    | NL_CALL of nl_exp * nl_exp

  let empty_env = []
  let rec apply_env : env -> var -> int
  =fun env x ->
  match env with
  |[] -> raise(Failure "Type Error : there's no variable in environment")
  |hd::tl -> if(hd=x) then 0 else 1+(apply_env tl x)

  let rec trans : exp -> env -> nl_exp
  =fun exp env ->
  match exp with
  |CONST n -> NL_CONST n
  |VAR x -> NL_VAR (apply_env env x)
  |ADD (e1,e2) -> NL_ADD(trans e1 env,trans e2 env)
  |SUB (e1,e2) -> NL_SUB(trans e1 env,trans e2 env) 
  |ISZERO e -> NL_ISZERO(trans e env)
  |IF (e1,e2,e3) -> NL_IF((trans e1 env),(trans e2 env),(trans e3 env))
  |LET (x,e1,e2) -> NL_LET((trans e1 env),(trans e2 (x::env)))
  |PROC (x,e) -> NL_PROC(trans e (x::env))
  |CALL (e1,e2) -> NL_CALL(trans e1 env, trans e2 env)

  let translate : program -> nl_program
  =fun pgm -> trans pgm empty_env
