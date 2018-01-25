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

  type env = string list
  
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
  
  let rec length : var -> env -> int
  = fun x env ->
    match env with
    | [] -> raise (Failure "Sentence error")
    | hd::tl -> if hd=x then 0 else 1 + length x tl

  let rec eval : exp -> env -> nl_exp
  =fun exp env ->
    match exp with
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (length x env)
    | ADD (e1,e2) -> NL_ADD(eval e1 env, eval e2 env)
    | SUB (e1,e2) -> NL_SUB(eval e1 env, eval e2 env)
    | ISZERO e -> NL_ISZERO(eval e env)
    | IF (e1,e2,e3) -> NL_IF(eval e1 env, eval e2 env, eval e3 env)
    | LET (x,e1,e2) -> NL_LET(eval e1 env, eval e2 (x::env))
    | PROC (x,e) -> NL_PROC(eval e (x::env))
    | CALL (e1,e2) -> NL_CALL(eval e1 env, eval e2 env)

  let translate : program -> nl_program
  =fun pgm -> eval pgm []
