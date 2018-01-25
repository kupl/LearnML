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
  let nl_empty_env : nl_value list = []
  let rec apply_n1_env : nl_env -> int -> nl_value
  =fun env n ->
  match env with
  |[] -> raise(Failure "Type Error : exceeding index or blank link")
  |hd::tl -> if (n=0) then (hd) else (apply_n1_env tl (n-1))


  let rec nl_eval nl_exp env =
  match nl_exp with
  |NL_CONST n -> NL_Int n
  |NL_VAR n -> apply_n1_env env n
  |NL_ADD(nl_e1,nl_e2) ->
    let n1 = nl_eval nl_e1 env in
    let n2 = nl_eval nl_e2 env in
    (match n1,n2 with
    |NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
    |_ -> raise(Failure "Type Error: non-numeric values"))
  |NL_SUB(nl_e1,nl_e2) ->
    let n1 = nl_eval nl_e1 env in
    let n2 = nl_eval nl_e2 env in
    (match n1,n2 with
    |NL_Int n1, NL_Int n2 -> NL_Int (n1-n2)
    |_ -> raise(Failure "Type Error: non-numeric values"))
  |NL_ISZERO nl_e ->
  (match nl_eval nl_e env with
    |NL_Int n when n=0 -> NL_Bool true
    |_ -> NL_Bool false)
  |NL_IF(nl_e1,nl_e2,nl_e3) ->
  (match nl_eval nl_e1 env with
    |NL_Bool true -> nl_eval nl_e2 env
    |NL_Bool false -> nl_eval nl_e3 env
    |_ -> raise(Failure "Type Error: condition must be Bool type"))
  |NL_LET(nl_e1,nl_e2) ->
    let v1 = nl_eval nl_e1 env in
      nl_eval nl_e2 (v1::env)
  |NL_PROC nl_e -> NL_Procedure (nl_e,env)
  |NL_CALL (nl_e1, nl_e2) ->
  (match nl_eval nl_e1 env with
    |NL_Procedure (nl_e,env2) ->
      let v = nl_eval nl_e2 env in
        nl_eval nl_e (v::env2)
    |_ -> raise(Failure "Type Error: condition must be Procedure type")) 

  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm nl_empty_env