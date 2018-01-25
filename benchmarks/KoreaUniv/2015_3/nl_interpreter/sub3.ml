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
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0

  let rec nle_bop : (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
  =fun op e1 e2 env ->(
    let v1 = nle(e1,env) in
    let v2 = nle(e2,env) in
      (match v1,v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
      | _ -> raise (Failure "Type Error: non-numeric values")))

  and nle : nl_exp * nl_env -> nl_value
  =fun (exp,env) ->
    match exp with
    | NL_CONST n -> NL_Int n
    | NL_VAR n -> List.nth env n
    | NL_ADD (e1,e2) -> nle_bop (+) e1 e2 env
    | NL_SUB (e1,e2) -> nle_bop (-) e1 e2 env
    | NL_ISZERO e ->
      (match nle(e,env) with
      | NL_Int n when n = 0 -> NL_Bool true
      | _ -> NL_Bool false)
    | NL_IF (e1,e2,e3) ->
      (match nle(e1,env) with
      | NL_Bool true -> nle(e2,env)
      | NL_Bool false -> nle(e3,env)
      | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | NL_LET (e1,e2) ->
      let v1 = nle(e1,env) in nle(e2,([v1]@env))
    | NL_PROC(e) -> NL_Procedure(e,env)
    | NL_CALL(e1,e2) ->
      (match nle(e1,env) with | NL_Procedure(e,en) -> let v1 = nle(e2,env) in nle(e,[v1]@en))

  let nl_run : nl_program -> nl_value
  =fun nlp -> nle(nlp,[])