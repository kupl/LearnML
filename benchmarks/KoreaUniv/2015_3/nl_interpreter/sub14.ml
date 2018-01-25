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

let rec find_n: int * nl_env -> nl_value
= fun (n, env) ->
match env with
[] -> raise (Failure "error")
|hd::tl -> if n=0 then hd else find_n((n-1), tl)

let rec get: nl_program * nl_env -> nl_value
= fun (pgm, env) ->
match pgm with
NL_CONST n -> NL_Int n
|NL_VAR n -> find_n (n, env)
|NL_ADD (p1, p2) -> (match get (p1, env), get (p2, env) with
	NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
	|_ -> raise (Failure "error in NL_ADD"))
|NL_SUB (p1, p2) -> (match get (p1, env), get (p2, env) with
	NL_Int n1, NL_Int n2  -> NL_Int (n1-n2)
	|_-> raise (Failure "error in NL_SUB"))
|NL_ISZERO p1 -> (match get(p1, env) with
	NL_Int n -> if n=0 then NL_Bool true else NL_Bool false
	|_-> raise (Failure "error in NL_ISZERO"))
|NL_IF (p1, p2, p3) -> (match get(p1, env) with
	NL_Bool tf -> if tf=true then get(p2, env) else get(p3, env)
	|_-> raise (Failure "error in NL_IF"))
|NL_LET (p1, p2) -> let v1 = get (p1, env) in get (p2, v1::env)
|NL_PROC p1 -> NL_Procedure (p1, env)
|NL_CALL (p1, p2) -> let v2 = get (p2, env) in let v1 = get (p1, env) in 
	(match v1 with
	NL_Procedure (p_v1, env_v1) -> get (p_v1, v2::env_v1)
	|_-> raise (Failure "error in NL_CALL"))


let nl_run : nl_program -> nl_value
=fun pgm -> get (pgm, [])
