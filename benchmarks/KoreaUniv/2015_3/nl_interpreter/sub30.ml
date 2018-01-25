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
 
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list
  
  
  let extend_env x e = x::e
  let rec apply_env n e =
    match e with
    | [] -> raise Not_found
    | h::t -> if n = 0 then h else apply_env (n-1) t
  
  
  let rec eval_bop : (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
  =fun op e1 e2 env ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  and eval : nl_exp -> nl_env -> nl_value
  =fun exp env ->
    match exp with
    | NL_CONST n -> NL_Int n
    | NL_VAR n -> apply_env n env
    | NL_ADD (e1, e2) -> eval_bop (+) e1 e2 env
    | NL_SUB (e1, e2) -> eval_bop (-) e1 e2 env
    | NL_ISZERO e ->
      (let v = eval e env in
        match v with
        | NL_Int n -> if n = 0 then NL_Bool true else NL_Bool false
        | _ -> raise (Failure "Type Error: subexpression of zero? must be Int type"))
    | NL_IF (c_exp, t_exp, f_exp) ->
      (match eval c_exp env with
      | NL_Bool true -> eval t_exp env
      | NL_Bool false -> eval f_exp env
      | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | NL_LET (v_exp, e) ->
      let v_val = eval v_exp env in
        eval e (extend_env v_val env)
    | NL_PROC e -> NL_Procedure (e, env)
    | NL_CALL (f_exp, x_exp) ->
      (let f_val = eval f_exp env in
        match f_val with
        | NL_Procedure (e, env1) -> let x_val = eval x_exp env in eval e (extend_env x_val env1)
	| _ -> raise (Failure "Type Error: call type error"))
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> eval pgm []