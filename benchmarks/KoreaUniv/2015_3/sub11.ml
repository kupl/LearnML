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
  
  let rec get_weight : branch -> weight
  =fun b -> 
	match b with
		  SimpleBranch (l,w) -> w
		| CompoundBranch (l,(b1,b2)) -> (get_weight b1) + (get_weight b2)

  let get_length : branch -> int
  =fun b -> match b with 
		  SimpleBranch (l,w) -> l
		| CompoundBranch (l,m) -> l

  let cal_torque : branch -> int
  =fun b -> (get_length b) * (get_weight b)
(*
  let rec check_balanced : branch -> bool
  =fun b -> 
	match b with
		  SimpleBranch (l,w) -> true
		| CompoundBranch (l, (b1,b2)) -> (check_balanced b1) && (check_balanced b2) 
	
  let balanced : mobile -> bool
  =fun (lb,rb) -> 
	let tlb = cal_torque lb in
	let trb = cal_torque rb in
		if tlb = trb then (check_balanced lb && check_balanced rb) else false
*)
  let check_torque : mobile -> bool
  =fun (lb,rb) -> cal_torque lb = cal_torque rb 

  let rec sub_balanced : branch -> bool
  =fun b -> 
	match b with
		| SimpleBranch (_,_) -> true
		| CompoundBranch (l,m) -> check_balanced m 	
	
  and check_balanced : mobile -> bool
  =fun (lb,rb) -> 
	let tq = check_torque (lb,rb) in
	let islb = sub_balanced lb in
	let isrb = sub_balanced rb in
		tq && islb && isrb

  let balanced : mobile -> bool
  =fun (lb,rb) -> check_balanced (lb,rb)
 

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec is_well_formed : exp -> var list -> bool
  =fun e env -> 
	match e with
		| V v -> if (List.mem v env) then true else false
		| P (v,exp') -> let env' = v::env in (is_well_formed exp' env')
		| C (exp1, exp2) -> 
			let r1 = is_well_formed exp1 env in
			let r2 = is_well_formed exp2 env in
				r1 && r2

  let check : exp -> bool
  =fun e -> is_well_formed e [] 

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
  
  let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
=fun op e1 e2 env ->
	let v1 = eval e1 env in
  	let v2 = eval e2 env in
		(match v1,v2 with
		| Int n1, Int n2 -> Int (op n1 n2)
		| _ -> raise (Failure "Type Error: non-numeric values"))
 
and eval : exp -> env -> value
=fun exp env ->
	match exp with
		| CONST n -> Int n
		| VAR x -> apply_env env x
		| ADD (e1,e2) -> eval_bop (+) e1 e2 env
		| SUB (e1,e2) -> eval_bop (-) e1 e2 env
		| ISZERO e ->
			(match eval e env with
				| Int n when n = 0 -> Bool true
				| _ -> Bool false)
		| IF (e1,e2,e3) ->
			(match eval e1 env with
				| Bool true -> eval e2 env
				| Bool false -> eval e3 env
				| _ -> raise (Failure "Type Error: condition must be Bool type"))
		| LET (x,e1,e2) ->
			let v1 = eval e1 env in
			eval e2 (extend_env (x,v1) env)
		| LETREC (f,x,e1,e2) -> eval e2 (extend_env (f, RecProcedure (f,x,e1,env)) env) 
		| PROC (x,e) -> Procedure (x, e, env)
		| CALL (e1,e2) ->  
			let v = eval e2 env in
			let e' = eval e1 env in
				match e' with
					| Procedure (x, e, env') -> eval e (extend_env (x, v) env')
					| RecProcedure (f, x, e, env') -> eval e (extend_env (x, v) (extend_env (f, RecProcedure(f,x,e,env')) env'))
					| _ -> raise (Failure "Type Error: it must be Procedure or RecProcedure") 

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

  exception NoSuchArgument

  let rec search_index : var list -> var -> int -> int
  =fun l v i ->
	match l with
		| [] -> raise NoSuchArgument 
		| hd::tl -> if hd = v then i else search_index tl v (i+1)

  let index_of : var list -> var -> int
  =fun l v -> search_index l v 0 

  let rec converter : program -> var list -> nl_program
  =fun e env ->
	match e with 
		| CONST i -> NL_CONST i
		| VAR v -> NL_VAR (index_of env v) 
		| ADD (e1,e2) -> NL_ADD ((converter e1 env), (converter e2 env))
		| SUB (e1,e2) -> NL_SUB ((converter e1 env), (converter e2 env))
		| ISZERO e -> NL_ISZERO (converter e env) 
		| IF (e1,e2,e3) -> NL_IF ((converter e1 env), (converter e2 env), (converter e3 env)) 
		| LET (x,e1,e2) -> NL_LET ((converter e1 env), (converter e2 (x::env)))
		| PROC (x,e) -> NL_PROC (converter e (x::env))
		| CALL (e1,e2) -> NL_CALL ((converter e env), (converter e env)) 

  let translate : program -> nl_program
  =fun pgm -> converter pgm []
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
end
