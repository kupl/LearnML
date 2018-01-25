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

let rec nl_lookup_env : nl_value list -> int -> nl_value
	= fun l n -> match l with
	[] -> raise (Failure "Wrong Environment")
	|h::t -> if n = 0 then h else nl_lookup_env t (n-1)

	and nl_op : (int->int->int)->nl_exp->nl_exp->nl_value list->nl_value
	= fun op e1 e2 env->
		let v1 = nl_interpret e1 env in
		let v2 = nl_interpret e2 env in
			(match v1, v2 with
				NL_Int i1, NL_Int i2 -> NL_Int (op i1 i2)
				|_ -> raise (Failure "Type error: value should be NL_Int"))

	and nl_interpret : nl_program -> nl_value list -> nl_value
	= fun exp env -> match exp with
	|NL_CONST i -> NL_Int i
	|NL_VAR i -> nl_lookup_env env i
	|NL_ADD (e1, e2) -> nl_op (+) e1 e2 env
	|NL_SUB (e1, e2) -> nl_op (-) e1 e2 env
	|NL_ISZERO e1 -> (match nl_interpret e1 env with
		NL_Int 0 -> NL_Bool true
		|NL_Int _ -> NL_Bool false
		|_ -> raise (Failure "Type error: value should be NL_Int"))
	|NL_IF (e1, e2, e3) -> (match nl_interpret e1 env with
		|NL_Bool true -> nl_interpret e2 env
		|NL_Bool false -> nl_interpret e3 env
		|_ -> raise (Failure "Type error: expression 1 should be NL_Bool"))
	|NL_LET	(e1, e2) -> nl_interpret e2 ((nl_interpret e1 env)::env)
	|NL_PROC e1 -> NL_Procedure (e1, env)
	|NL_CALL (e1, e2) -> (match nl_interpret e1 env with
		|NL_Procedure (e, env1)->nl_interpret e ((nl_interpret e2 env)::env1)
		|_ -> raise (Failure "Type error: Must call NL_Procedure"))
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_interpret pgm []
