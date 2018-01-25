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
  
  exception InvalidArgument
  
  let rec nl_eval : nl_program -> nl_env -> nl_value
  =fun nle env ->
	match nle with
		| NL_CONST i -> NL_Int i 
		| NL_VAR n -> List.nth env n
		| NL_ADD (nle1,nle2) -> 
			let v1 = nl_eval nle1 env in
			let v2 = nl_eval nle2 env in
			(match v1,v2 with
				| NL_Int i1, NL_Int i2 -> NL_Int (i1+i2)
				| _ -> raise InvalidArgument)
		| NL_SUB (nle1,nle2) ->
			let v1 = nl_eval nle1 env in
			let v2 = nl_eval nle2 env in
			(match v1,v2 with
				| NL_Int i1, NL_Int i2 -> NL_Int (i1-i2)
				| _ -> raise InvalidArgument)  
		| NL_ISZERO nle -> 
			if (nl_eval nle env) = NL_Int 0 
				then NL_Bool (true) 
				else NL_Bool (false)
		| NL_IF (nle1,nle2,nle3) -> 
			if (nl_eval nle1 env) = NL_Bool (true) 
				then (nl_eval nle2 env) 
				else (nl_eval nle3 env)
		| NL_LET (nle1,nle2) -> 
			let v1 = (nl_eval nle1 env) in
				(nl_eval nle2 (v1::env))
		| NL_PROC nle -> NL_Procedure (nle,env)
		| NL_CALL (nle1,nle2) -> 
			let v = (nl_eval nle2 env) in
			let e' = (nl_eval nle1 env) in
				(match e' with
					| NL_Procedure (nle',env') ->
						nl_eval nle' (v::env')
					| _ -> raise InvalidArgument) 

  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm []