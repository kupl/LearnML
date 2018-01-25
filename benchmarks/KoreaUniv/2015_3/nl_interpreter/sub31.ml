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

  let rec find_var 
  =fun idx lst ->
    match lst with
    | h::t -> if idx=0 then h else find_var (idx-1) t
    | _ -> raise (Failure "Environment is empty")

  let rec nl_eval : nl_exp -> nl_env -> nl_value
  =fun nexp nenv -> 
    match nexp with
    | NL_CONST n -> NL_Int n 
    | NL_VAR n -> find_var n nenv  
    | NL_ADD (ne1,ne2) ->
      let v1 = nl_eval ne1 nenv in
      let v2 = nl_eval ne2 nenv in
      (match v1,v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int (n1 + n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
    | NL_SUB (ne1,ne2) -> 
      let v1 = nl_eval ne1 nenv in
      let v2 = nl_eval ne2 nenv in
        (match v1,v2 with
        | NL_Int n1, NL_Int n2 -> NL_Int (n1 - n2)
        | _ -> raise (Failure "Type Error: non-numeric values"))
    | NL_ISZERO (ne) -> 
      (match nl_eval ne nenv with
      | NL_Int n when n = 0 -> NL_Bool true
      | _ -> NL_Bool false)
    | NL_IF (ne1,ne2,ne3) ->
      (match nl_eval ne1 nenv with
      | NL_Bool true -> nl_eval ne2 nenv
      | NL_Bool false -> nl_eval ne3 nenv
      | _ -> raise (Failure "Type Error: condition must be NL_Bool type"))
    | NL_LET (ne1,ne2) ->
      let v1 = nl_eval ne1 nenv in
        nl_eval ne2 (v1::nenv)
    | NL_PROC (ne) -> NL_Procedure (ne,nenv) 
    | NL_CALL (ne1,ne2) ->
      let e = nl_eval ne1 nenv in 
      let v = nl_eval ne2 nenv in
      (match e with
      | NL_Procedure (npe, npenv) -> nl_eval npe (v::npenv)
      | _ -> raise (Failure "Type Error: non-procedure"))

  let nl_run : nl_program -> nl_value
  =fun pgm -> 
    let env = [] in
    nl_eval pgm env
