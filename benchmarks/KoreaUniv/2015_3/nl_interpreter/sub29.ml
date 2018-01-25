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

let empty_env = fun _ -> raise (Failure "Environment is empty")
let extend_env v e= v::e 

let rec eval: nl_exp->nl_env->nl_value
=fun exp env ->match exp with
    |NL_CONST n->NL_Int n
    |NL_VAR n->List.nth env n
    |NL_ADD (e1,e2)->let v1=eval e1 env in let v2= eval e2 env in (match v1,v2 with
          | NL_Int n1,NL_Int n2-> NL_Int (n1 + n2)
          |_->raise (Failure "Type Error: non-numeric values"))
    |NL_SUB (e1,e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with 
                  | NL_Int n1, NL_Int n2 -> NL_Int (n1 - n2) 
                  | _ -> raise (Failure "Type Error: non-numeric values"))
    |NL_ISZERO e -> (match eval e env with | NL_Int n when n = 0 -> NL_Bool true |_ ->NL_Bool false)
    |NL_IF (e1,e2,e3) -> (match eval e1 env with 
        | NL_Bool true -> eval e2 env 
        | NL_Bool false -> eval e3 env 
        | _ -> raise (Failure "Type Error: condition must be NL_Bool type"))
    |NL_LET (e1,e2)-> let v1=eval e1 env in eval e2 (extend_env v1 env)
    |NL_PROC (e1)->NL_Procedure (e1, env)
    |NL_CALL (e1,e2)->(match eval e1 env with | NL_Procedure (e,env2)->(match eval e2 env with |v2-> eval e (extend_env v2 env2))
               |_->raise (Failure "Type Error: condition must be Procedure") )
              

  let nl_run : nl_program -> nl_value
  =fun pgm -> eval pgm []
