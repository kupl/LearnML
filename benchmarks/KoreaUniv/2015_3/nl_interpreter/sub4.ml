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
	
	let rec find_value : int * nl_value list -> nl_value 
	= fun (x,v_l) ->
		match v_l with
		h::t-> if x=0 then h else find_value (x-1,t)
	| _ -> raise (Failure "not bound value")						

	let rec cal_nl : nl_program * nl_env -> nl_value
	= fun (pgm,env) -> 
		match pgm with
		NL_CONST n -> NL_Int n
	| NL_VAR n -> find_value (n,env)
	| NL_ADD (e1,e2) ->
			let v1 = cal_nl (e1,env) in
			let v2 = cal_nl (e2,env) in
				(match v1,v2 with
				NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
			| _ -> raise (Failure "Type error: non-numeric values"))
	| NL_SUB (e1,e2) ->
			let v1 = cal_nl (e1,env) in
			let v2 = cal_nl (e2,env) in
				(match v1,v2 with
				NL_Int n1, NL_Int n2 -> NL_Int (n1-n2)
			| _ -> raise (Failure "Type error: non-numeric values"))
	| NL_ISZERO e -> 
			let v1 = cal_nl (e,env) in
				(match v1 with
				NL_Int n -> if n=0 then NL_Bool true else NL_Bool false
			| _ -> NL_Bool false)
	| NL_IF (e1,e2,e3) ->
			(match cal_nl (e1,env) with
			NL_Bool true -> cal_nl (e2,env)
		| NL_Bool false -> cal_nl (e3,env)
		| _ -> raise (Failure "Type error: codition must be Bool type"))
	| NL_LET (e1,e2) -> 
			let v1 = cal_nl (e1,env) in
				cal_nl (e2,v1::env)
	| NL_PROC e -> NL_Procedure (e,env)
	| NL_CALL (e1,e2) ->
		let v1 = cal_nl (e1,env) in
		let v2 = cal_nl (e2,env) in
			(match v1 with
			NL_Procedure (e2,env1) -> cal_nl (e2,v2::env1)
		| _ -> raise (Failure "first reference must be NL_Procedure"))

	let nl_run : nl_program -> nl_value
	= fun pgm -> cal_nl (pgm,[])
