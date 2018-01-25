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
  
  
  type t = var -> int
  exception Not_found
  let empty = fun x-> prerr_endline x; raise Not_found
  let lookup x e = e x
  let extend x e = fun y -> if x = y then 0 else 1 + (lookup y e)
  
  let rec trans : exp -> t -> nl_program
  =fun exp env ->
    match exp with
    | CONST n -> NL_CONST n
    | VAR var_x -> NL_VAR (lookup var_x env)
    | ADD (exp_1, exp_2) -> NL_ADD ((trans exp_1 env), (trans exp_2 env))
    | SUB (exp_1, exp_2) -> NL_SUB ((trans exp_1 env), (trans exp_2 env))
    | ISZERO exp_c -> NL_ISZERO (trans exp_c env)
    | IF (exp_c, exp_t, exp_f) -> NL_IF (trans exp_c env, trans exp_t env, trans exp_f env)
    | LET (var_x, exp_v, exp_e) -> NL_LET (trans exp_v env, trans exp_e (extend var_x env))
    | PROC (var_x, exp_1) -> NL_PROC (trans exp_1 (extend var_x env))
    | CALL (exp_1, exp_2) -> NL_CALL (trans exp_1 env, trans exp_2 env)
  
  let translate : program -> nl_program
  =fun pgm -> trans pgm empty
