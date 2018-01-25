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

  let rec get_value : int -> nl_env -> nl_value
  = fun count nl_env ->
    match nl_env with
    | [] -> raise (Failure "Sentence error")
    | hd::tl -> if count=0 then 
      (match hd with
      | NL_Int x -> NL_Int x
      | NL_Bool x -> raise (Failure "Error")
      | NL_Procedure(x,y) -> raise (Failure "Error"))
      else get_value (count-1) tl

  let rec eval2 : nl_exp -> nl_env -> nl_value
  = fun nl_exp nl_env ->
    match nl_exp with
    | NL_CONST n -> NL_Int n
    | NL_VAR count -> (get_value count nl_env)
    | NL_ADD (e1,e2) ->
      let v1 = eval2 e1 nl_env in
      let v2 = eval2 e2 nl_env in
        (match v1,v2 with
        | NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
        | _ -> raise (Failure "Type Error: non-numeric values"))
    | NL_SUB (e1,e2) ->
      let v1 = eval2 e1 nl_env in
      let v2 = eval2 e2 nl_env in
        (match v1,v2 with
        | NL_Int n1, NL_Int n2 -> NL_Int (n1-n2)
        | _ -> raise (Failure "Type Error: non-numeric values"))
    | NL_ISZERO e ->
      (match eval2 e nl_env with
      | NL_Int n when n = 0 -> NL_Bool true
      | _ -> NL_Bool false)
    | NL_IF(e1,e2,e3) ->
      (match eval2 e1 nl_env with
      | NL_Bool true -> eval2 e2 nl_env
      | NL_Bool false -> eval2 e3 nl_env
      | _ -> raise (Failure "Type Error: condition must be NL_Bool type"))
    | NL_LET(e1,e2) ->
      let v1 = eval2 e1 nl_env in
        eval2 e2 (v1::nl_env)
    | NL_PROC e -> NL_Procedure(e,nl_env)
    | NL_CALL(e1,e2) -> 
      let p = eval2 e1 nl_env in
        (match p with
        | NL_Int x -> raise (Failure "error")
        | NL_Bool x -> raise (Failure "error")
        | NL_Procedure(body,nl_env) ->
          (let arg = eval2 e2 nl_env in
          eval2 body (arg::nl_env)))
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> eval2 pgm []
