
(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

module Problem1 =struct
	type mobile = branch * branch
	and branch = SimpleBranch of length * weight
    	       | CompoundBranch of length * mobile
	and length = int
	and weight = int

	let rec eval1 : mobile -> int
	= fun e -> 
		match e with
		| (SimpleBranch(a,b), SimpleBranch(c,d)) ->
			if a*b=c*d then b+d else -1
		| (CompoundBranch(a,b), SimpleBranch(c,d)) ->
			eval1(SimpleBranch(a, eval1(b)), SimpleBranch(c,d))
		| (SimpleBranch(a,b), CompoundBranch(c,d)) ->
			eval1(SimpleBranch(a,b), SimpleBranch(c, eval1(d)))
		| (CompoundBranch(a,b), CompoundBranch(c,d)) ->
			eval1(SimpleBranch(a, eval1(b)), SimpleBranch(c, eval1(d)))

	let balanced : mobile -> bool
	=fun (lb,rb) -> if eval1(lb,rb)=(-1) then false else true
end
(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
	type exp = V of var
			 | P of var * exp
			 | C of exp * exp
	and var = string
	
	let check : exp -> bool
	=fun e ->
		let rec check_help m n_list = 
			match m with
			| V n -> List.mem n n_list
			| P (n, e1) -> check_help e1 (n::n_list)
			| C (e1,e2) -> check_help e1 n_list && check_help e2 n_list
		in check_help e []
end

(***********************************)
(**            Problem 3          **)
(***********************************)
module Problem3 = struct
	type program = exp
	and exp = 
		| CONST of int
		| VAR of var
		| ADD of exp * exp
		| SUB of exp * exp
		| ISZERO of exp
		| IF of exp * exp * exp
		| LET of var * exp * exp
		| LETREC of var * var * exp * exp
		| PROC of var * exp
		| CALL of exp * exp
	and var = string

	type value = Int of int | Bool of bool 
				| Procedure of var * exp * env 
				| RecProcedure of var * var * exp * env
	and env = var -> value

	let empty_env = fun _ -> raise (Failure "Environment is empty")
	let extend_env (x,v) e = fun y -> if x = y then v else (e y)
	let apply_env e x = e x
	
	let rec eval : exp -> env -> value
	=fun exp env ->
		match exp with
		| CONST n -> Int n
		| VAR x -> apply_env env x
		| ADD (e1,e2) ->
			let v1 = eval e1 env in
			let v2 = eval e2 env in
				(
				match v1, v2 with
				| Int n1, Int n2 -> Int (n1+n2)
				| _ -> raise (Failure "Type Error: non-numeric value")
				)
		| SUB (e1,e2) ->
			let v1 = eval e1 env in
			let v2 = eval e2 env in
				(
				match v1, v2 with
				| Int n1, Int n2 -> Int (n1-n2)
				| _ -> raise (Failure "Type Error: non-numeric value")
				)
		| ISZERO e ->
			(
			match eval e env with
			| Int n when n=0 -> Bool true
			| _ -> Bool false
			)
		| IF (e1,e2,e3) ->
			(
			match eval e1 env with
			| Bool true -> eval e2 env
			| Bool false -> eval e3 env
			| _ -> raise (Failure "Type Error: condition must be Bool type")
			)
		| LET (x,e1,e2) ->
			let v1 = eval e1 env in
				eval e2 (extend_env (x,v1) env)
		| LETREC (f,x,e1,e2) ->
			let env = extend_env (f, RecProcedure(f,x,e1,env)) env in
				eval e2 env
		| PROC (v, e) ->
			Procedure (v,e,env)
		| CALL (e1,e2) ->
			let arg = eval e2 env in
			let func = eval e1 env in
				(
				match func with
				| Procedure (param, body, env1) ->
					eval body (extend_env (param, arg) env1)
				| RecProcedure (name, param, body, env1) ->
					eval body (extend_env (param, arg) (extend_env (name,func) env1))
				| _-> raise (Failure "Call Func Error")
				)

	let run : program -> value
	=fun pgm -> eval pgm empty_env
end
(***********************************)
(**            Problem 4          **)
(***********************************)
module Problem4 = struct
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
end
(***********************************)
(**            Problem 5          **)
(***********************************)
module Problem5 = struct
	open Problem4
	type nl_value = NL_Int of int 
				| NL_Bool of bool 
				| NL_Procedure of nl_exp * nl_env
	and nl_env = nl_value list
	
	let empty_nl_env = []
	let extend_nl_env x k = x::k
	let rec apply_nl_env x k =
		match k with
		| [] -> raise (Failure "empty")
		| hd::tl -> if x=0 then hd else (apply_nl_env (x-1) tl)
	
	let rec nl_eval
	=fun exp env ->
		match exp with
		| NL_CONST n -> NL_Int n
		| NL_VAR x -> apply_nl_env x env
		| NL_ADD (e1,e2) ->
			let v1 = nl_eval e1 env in
			let v2 = nl_eval e2 env in
				(
				match v1, v2 with
				| NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
				| _ -> raise (Failure "Type Error: non-numeric value")
				)
		| NL_SUB (e1,e2) ->
			let v1 = nl_eval e1 env in
			let v2 = nl_eval e2 env in
				(
				match v1, v2 with
				| NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
				| _-> raise (Failure "Type Error: non-numeric value")
				)
		| NL_ISZERO e ->
			(
			match nl_eval e env with
			| NL_Int n when n=0 -> NL_Bool true
			| _ -> NL_Bool false
			)
		| NL_IF (e1,e2,e3) ->
			(
			match nl_eval e1 env with
			| NL_Bool true -> nl_eval e2 env
			| NL_Bool false -> nl_eval e3 env
			| _ -> raise (Failure "Type Error: condition must be NL_Bool type")
			)
		| NL_LET (e1,e2) ->
			let v = nl_eval e1 env in
				nl_eval e2 (extend_nl_env v env)
		| NL_PROC e ->
			NL_Procedure (e, env)
		| NL_CALL (e1,e2) ->
			let arg = nl_eval e2 env in
			let func = nl_eval e1 env in
				(
				match func with
				| NL_Procedure (e, env1) ->
					nl_eval e (extend_nl_env arg env1)
				| _ -> raise (Failure "Func Error")
				)

	let nl_run : nl_program -> nl_value
	=fun pgm -> nl_eval pgm empty_nl_env
end
