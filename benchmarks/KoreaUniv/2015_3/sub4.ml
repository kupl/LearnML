(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)
module Problem1 = struct
	type mobile = branch * branch
	and branch = SimpleBranch of length * weight
           	| CompoundBranch of length * mobile
	and length = int
	and weight = int

	let rec balanced : mobile -> bool
	= fun (lb,rb) -> false

	let rec cal_weight : branch -> weight
	= fun b ->
		match b with
		SimpleBranch (l,w) -> w
	| CompoundBranch (l,m) -> 
			(match m with
				(lb,rb) -> (cal_weight lb) + (cal_weight rb));;

	let rec cal_torque : branch -> int
	= fun b ->
		match b with
		SimpleBranch (l,w) -> l * w
	|	CompoundBranch (l,m) -> 
			(match m with
				(lb,rb) -> l * (cal_weight lb + cal_weight rb));;

	let rec balanced : mobile -> bool
	= fun (lb,rb) ->
		match lb,rb with
		SimpleBranch (l1,w1),SimpleBranch (l2,w2) -> 
			if cal_torque lb = cal_torque rb then true else false
	| SimpleBranch (l1,w),CompoundBranch (l2,m) ->
			if (balanced m = true) && (cal_torque lb = cal_torque rb)
				then true else false
	| CompoundBranch (l1,m),SimpleBranch (l2,w) -> 
			if (balanced m = true) && (cal_torque lb = cal_torque rb)
				then true else false
	| CompoundBranch (l1,m1),CompoundBranch (l2,m2) ->
			if (balanced m1 = true) && (balanced m2 = true) 
			&& (cal_torque lb = cal_torque rb)
				then true else false;;
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check : exp -> bool
  = fun e -> true

	let rec check_2 : exp*(var list) -> bool
	= fun (e,l) ->
	match e with
	V v ->
		(match l with
		[] -> false
	|	h::t -> if v=h then true else check_2 (V v,t))
| P (v,e1) -> check_2 (e1, v::l)
| C (e1,e2) -> if check_2 (e1,l)=true && check_2 (e2,l)=true then true
								else false;;

	let check : exp -> bool
	= fun e -> check_2 (e,[]);;

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
  let extend_env (x,v) env = fun y -> if x = y then v else (env y)
  let apply_env env x = env x

	let rec eval : exp -> env -> value
	= fun exp env ->
		match exp with
		CONST n -> Int n
	| VAR x -> apply_env env x
	| ADD (e1,e2) -> 
		let v1 = eval e1 env in
		let v2 = eval e2 env in
			(match v1,v2 with
			Int n1, Int n2 -> Int (n1+n2)
		|	_ -> raise (Failure "Type Error: non-numeric values"))
	| SUB (e1,e2) ->
		let v1 = eval e1 env in
		let v2 = eval e2 env in
			(match v1,v2 with
			Int n1, Int n2 -> Int (n1-n2)
		| _ -> raise (Failure "Type Error: non-numeric values"))
	| ISZERO e ->
			let v = eval e env in
				(match v with
				Int n -> if n=0 then Bool true else Bool false
			| _ -> Bool false)
	| IF (e1,e2,e3) ->
			(match eval e1 env with
			Bool true -> eval e2 env
		| Bool false -> eval e3 env
		| _ -> raise (Failure "Type Error: codition must be Bool type"))
	| LET (x,e1,e2) ->
			let v1 = eval e1 env in
				eval e2 (extend_env (x,v1) env)
	| LETREC (f,x,e1,e2) ->
			eval e2 (extend_env (f,RecProcedure (f,x,e1,env)) env) 
	| PROC (x,e) -> Procedure (x,e,env)
	| CALL (e1,e2) -> 
			let v1 = eval e1 env in
			let v2 = eval e2 env in
				(match v1 with
				Procedure (x,e3,env1) -> 
					let env2 = extend_env (x,v2) env1 in
						eval e3 env2
			| RecProcedure (f,x,e3,env1) -> 
					let env2 = extend_env (f,RecProcedure (f,x,e3,env1)) env1 in
					let env3 = extend_env (x,v2) env2 in
						eval e3 env3
			| _ ->
					 raise (Failure "first reference must be Procedure or RecProcedure"))

	let run : program -> value
	= fun pgm -> eval pgm empty_env
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

	let rec find_scope : string * string list * int -> int
	= fun (x,l,n) ->
		match l with
		[] -> 0
	|	h::t -> if x=h then n else find_scope (x,t,n+1)

	let rec translate2 : program * string list -> nl_program
	= fun (pgm,l) ->
		match pgm with
		CONST n -> NL_CONST n
	| VAR x -> NL_VAR (find_scope (x,l,0))
	| ADD (e1,e2) -> NL_ADD (translate2 (e1,l),translate2 (e2,l))
	| SUB (e1,e2) -> NL_SUB (translate2 (e1,l),translate2 (e2,l))
	| ISZERO e -> NL_ISZERO (translate2 (e,l))
	| IF (e1,e2,e3) -> NL_IF (translate2 (e1,l),translate2 (e2,l), translate2 (e3,l))
	| LET (x,e1,e2) -> NL_LET (translate2 (e1,l),translate2 (e2,x::l))
	| PROC (x,e) -> NL_PROC (translate2 (e,x::l))
	| CALL (e1,e2) -> NL_CALL (translate2 (e1,l),translate2 (e2,l))

	let rec translate : program -> nl_program
	= fun pgm -> translate2 (pgm,[])

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
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0
	
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
	
end
