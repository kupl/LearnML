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


let rec find2 : int * nl_value list -> nl_value
=fun (index,li) -> match li with
 |[] -> raise (Failure "Environment Search Error")
 |h::t -> if index=0 then h else find2(index-1,t)

let rec rr : nl_program -> nl_env -> nl_value
=fun nl_exp nl_env -> match nl_exp with
 NL_CONST i -> NL_Int i
|NL_VAR i -> find2 (i,nl_env)
|NL_ADD (e1,e2) ->
  let v1= rr e1 nl_env in
  let v2= rr e2 nl_env in
    (match v1,v2 with
    | NL_Int n1,NL_Int n2 -> NL_Int (n1+n2)
    | _ -> raise (Failure "Type ERROR AT NL_ADD"))
|NL_SUB (e1,e2) ->
  let v1= rr e1 nl_env in
  let v2= rr e2 nl_env in
    (match v1,v2 with
    | NL_Int n1,NL_Int n2 -> NL_Int (n1-n2)
    | _ -> raise (Failure "Type ERROR AT NL_SUB"))
|NL_ISZERO e -> let e1=rr e nl_env in
  (match e1 with
  | NL_Int x -> if x=0 then NL_Bool true else NL_Bool false
  | _ -> raise (Failure "Type Error at NL_ISZERO"))
|NL_IF (e1,e2,e3) -> let v1=rr e1 nl_env in
  (match v1 with
  | NL_Bool b -> if b=true then rr e2 nl_env else rr e3 nl_env
  | _ -> raise (Failure "Type Error at NL_IF"))
|NL_LET (e1,e2) -> let nl_env'=[rr e1 nl_env]@nl_env in rr e2 nl_env'
|NL_PROC e -> NL_Procedure(e,nl_env)
|NL_CALL (e1,e2) -> (match rr e1 nl_env with
  |NL_Procedure(e,nl_env') -> let nl_env2=[rr e2 nl_env]@nl_env' in rr e nl_env2
  | _ -> raise (Failure "Type Error at NL_CALL"))


let nl_run : nl_program -> nl_value
=fun pgm -> rr pgm []