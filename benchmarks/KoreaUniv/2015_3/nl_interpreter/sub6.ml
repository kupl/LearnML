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
  

  let rec getval : nl_env-> int -> nl_value
  = fun env i ->
  match env with
  []->raise (Failure "error")
  |h::t->if i=0 then h else getval t (i-1)

  let rec nl_doing : nl_exp-> nl_env->nl_value
  = fun exp env ->
    match exp with
    NL_CONST n->NL_Int n
    | NL_VAR x -> getval env x
    | NL_ADD (e1,e2) -> nl_doing_bop (+) e1 e2 env
    | NL_SUB (e1,e2) -> nl_doing_bop (-) e1 e2 env
    | NL_ISZERO e ->
      (match nl_doing e env with
      | NL_Int n -> NL_Bool ( n = 0)
      | _->NL_Bool false)
    | NL_IF (e1,e2,e3)->
      (match nl_doing e1 env with
        NL_Bool true->nl_doing e2 env
        |NL_Bool false->nl_doing e3 env
        |_->raise(Failure "Type error: it must be Bool type"))
    | NL_LET (e1,e2) ->
        let v1 = nl_doing e1 env in
        nl_doing e2 (v1::env)
    | NL_PROC (e1)->
        NL_Procedure (e1,env)
    | NL_CALL (e1, e2) ->
        (match nl_doing e1 env with
          NL_Procedure (e1',env') ->
            let v = nl_doing e2 env in
            nl_doing e1' (v::env)
          |_->raise(Failure "Type error: it must be nl_procedure type")
        )
  and nl_doing_bop : (int ->int ->int )->nl_exp->nl_exp->nl_env->nl_value
  =fun op e1 e2 env->
    let v1 = nl_doing e1 env in
    let v2 = nl_doing e2 env in
    (match v1,v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int(op n1 n2)
      | _->raise(Failure "Type Error: non-numeric values for op"))

    let nl_run : nl_program -> nl_value
  =fun pgm -> nl_doing pgm []