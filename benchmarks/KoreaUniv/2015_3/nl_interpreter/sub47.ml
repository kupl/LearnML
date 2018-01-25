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

	let rec rematch : (int * nl_env) -> nl_value
	= fun (n, l) -> match (n, l) with
		|(_, []) -> raise(Failure "Type Error: no value")
		|(0, h::t) -> h
		|(n', h::t) -> rematch (n'-1, t)

	let rec eval_bop : (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
	=fun op e1 e2 env -> 
	let v1 = eval e1 env in
	let v2 = eval e2 env in
		(match v1, v2 with
		| NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
		| _ -> raise(Failure "Type Error"))
  
	and eval : nl_exp -> nl_env -> nl_value
	= fun nl_exp env -> match nl_exp with
		|NL_CONST n -> NL_Int n
		|NL_VAR n -> rematch(n, env)
		|NL_ADD(nl_e1, nl_e2) -> eval_bop (+) nl_e1 nl_e2 env
		|NL_SUB(nl_e1, nl_e2) -> eval_bop (-) nl_e1 nl_e2 env
		|NL_ISZERO nl_e ->
			(let v = eval nl_e env in
				match v with
				|NL_Bool _ -> raise(Failure "Type Error")
				|NL_Int n -> if n = 0 then NL_Bool true else NL_Bool false
				|_ -> raise(Failure "Type Error"))
		|NL_IF(nl_e1, nl_e2, nl_e3) -> 
			(match eval nl_e1 env with
			|NL_Bool true -> eval nl_e2 env
			|NL_Bool false -> eval nl_e3 env
			|_ -> raise(Failure "Type Error")) 
		|NL_LET(nl_e1, nl_e2) ->
			let v1 = eval nl_e1 env in
				eval nl_e2 (v1::env)
		|NL_PROC(nl_e) -> NL_Procedure(nl_e, env)
		|NL_CALL(nl_e1, nl_e2) ->
			(match nl_e1 with
			NL_VAR n ->
				let v1 = eval nl_e1 env in (match v1 with
					|NL_Procedure(nl_e, env2) -> 
						let v = eval nl_e2 env in
							eval nl_e (v::env2)
					|_ -> raise(Failure "not function call"))
			|_ -> raise(Failure "Type Error: not function call"))

  let nl_run : nl_program -> nl_value
  =fun pgm -> eval pgm []
