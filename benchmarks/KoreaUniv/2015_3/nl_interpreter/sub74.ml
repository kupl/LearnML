type nl_program = nl_exp
and nl_exp = 
		|NL_CONST of int
		|NL_VAR of int
		|NL_ADD of nl_exp * nl_exp
		|NL_SUB of nl_exp * nl_exp
		|NL_ISZERO of nl_exp
		|NL_IF of nl_exp * nl_exp * nl_exp
		|NL_LET of nl_exp * nl_exp
		|NL_PROC of nl_exp
		|NL_CALL of nl_exp * nl_exp

type nl_value=
			 NL_Int of int
			|NL_Bool of bool
			|NL_Procedure of nl_exp * nl_env
and nl_env = nl_value list
let empty_nl_env = []
let rec apply_nl_env x e=
		match e with
		|[]->raise (Failure "Environment is empty!")
		|h::tl -> if x=0 then h else (apply_nl_env (x-1) tl)
let extend_nl_env x e1 = x::e1

let rec neval : nl_exp -> nl_env -> nl_value
=fun exp env->
		match exp with
		|NL_CONST(n) -> NL_Int(n)
		|NL_VAR(x)->apply_nl_env x env
		|NL_ADD(e1, e2)->
			let v1 = neval e1 env in
			let v2 = neval e2 env in
			(match v1, v2 with
			|NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
			|_->raise(Failure "Type error!"))
		|NL_SUB(e1, e2)->
			let v1 = neval e1 env in
			let v2 = neval e2 env in
			(match v1, v2 with
			|NL_Int n1, NL_Int n2 -> NL_Int (n1-n2)
			|_->raise (Failure "Type error!"))
		|NL_ISZERO(e)->
			(match neval e env with
			|NL_Int k when k=0 -> NL_Bool true
			|_-> NL_Bool false)
		|NL_IF (e1, e2, e3)->
			(match neval e1 env with
			|NL_Bool true -> neval e2 env
			|NL_Bool false -> neval e3 env
			|_-> raise (Failure "Type error!"))
		|NL_LET(e1, e2)->
			let v1 = neval e1 env in neval e2 (extend_nl_env v1 env)
		|NL_PROC(e)-> NL_Procedure (e, env)
		|NL_CALL(e1, e2)->
			let v1 = neval e1 env in
			let v2 = neval e2 env in
				(match v1 with
				|NL_Procedure (e1, e2)->neval e1 (extend_nl_env v2 e2)
				|_-> raise (Failure "Type error!"))

let nl_run : nl_program -> nl_value
=fun pgm -> neval pgm empty_nl_env
;;