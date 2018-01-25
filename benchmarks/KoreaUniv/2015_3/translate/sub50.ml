	type program = exp
	and exp = 
		| CONST of int
		| VAR of var
		| ADD of exp * exp
		| SUB of exp * exp
		| ISZERO of exp
		| IF of exp * exp * exp
		| LET of var * exp * exp
		| PROC of var * exp
		| CALL of exp * exp
	and var = string

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
	
	let tr_empty_env = []
	let tr_extend_env x k = x::k
	let rec tr_apply_env x k =
		match k with
		| [] -> raise (Failure "empty")
		| hd::tl -> if x=hd then 0 else 1+tr_apply_env x tl
	
	let rec eval 
	= fun exp env ->
		match exp with
		| CONST n -> NL_CONST n
		| VAR x -> NL_VAR (tr_apply_env x env)
		| ADD(e1,e2) -> 
			NL_ADD (eval e1 env, eval e2 env)
		| SUB(e1,e2) ->
			NL_SUB (eval e1 env, eval e2 env)
		| ISZERO e ->
			NL_ISZERO (eval e env)
		| IF (e1,e2,e3) ->
			NL_IF(eval e1 env, eval e2 env, eval e3 env)
		| LET (x,e1,e2) ->
			let env2 = tr_extend_env x env in
				NL_LET(eval e1 env, eval e2 env2)
		| PROC (x,e) ->
			NL_PROC (eval e (tr_extend_env x env))
		| CALL (e1,e2) ->
			NL_CALL (eval e1 env, eval e2 env)

	let translate : program -> nl_program
	=fun pgm -> eval pgm tr_empty_env
