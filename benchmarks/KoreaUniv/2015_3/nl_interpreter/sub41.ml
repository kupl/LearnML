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

  let nl_empty_env = []
  let nl_extend_env v e = v::e
  let rec nl_apply_env e x = match e with
	| hd::tl -> if x=0 then hd else (nl_apply_env tl (x-1))
	| [] -> raise (Failure "i don't know")

  let rec nl_eval_bop op e1 e2 nl_env =
	let v1 = nl_eval e1 nl_env in
	let v2 = nl_eval e2 nl_env in
		(match v1,v2 with
		| NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
		| _ -> raise (Failure "Type Error: non-numeric values"))
  and nl_eval: nl_exp -> nl_env -> nl_value
  =fun nl_exp nl_env -> match nl_exp with
     | NL_CONST n -> NL_Int n
     | NL_VAR x -> nl_apply_env nl_env x
     | NL_ADD (e1,e2) -> nl_eval_bop (+) e1 e2 nl_env
     | NL_SUB (e1,e2) -> nl_eval_bop (-) e1 e2 nl_env
     | NL_ISZERO e -> (match nl_eval e nl_env with
	| NL_Int n -> if n = 0 then NL_Bool true else NL_Bool false
	| _ -> raise (Failure "Type Error: subexpression of zero? must be Int type"))
     | NL_IF (e1,e2,e3) -> (match nl_eval e1 nl_env with
	| NL_Bool true -> nl_eval e2 nl_env
	| NL_Bool false -> nl_eval e3 nl_env
	| _ -> raise (Failure "Type Error: condition must be Bool type"))
     | NL_LET (e1,e2) ->
	let v1 = nl_eval e1 nl_env in
	nl_eval e2 (nl_extend_env v1 nl_env)
     | NL_PROC (x) -> NL_Procedure(x,nl_env)
     | NL_CALL (e1,e2) -> match nl_eval e1 nl_env with
	| NL_Procedure(ef,env2) -> nl_eval ef (nl_extend_env (nl_eval e2 nl_env) env2)
	| _ -> raise (Failure "Error: Given E1 is not Procedure!")

  let nl_run : nl_program -> nl_value = fun pgm -> nl_eval pgm nl_empty_env
