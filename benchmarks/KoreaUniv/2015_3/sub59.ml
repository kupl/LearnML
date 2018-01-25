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
  
  let rec sumweight : mobile -> int
  =fun (lb, rb) ->  match lb, rb with
  |SimpleBranch (ll, lw), SimpleBranch (rl, rw)-> lw + rw
  |CompoundBranch (ll, lm), SimpleBranch (rl, rw)-> (sumweight lm) + rw
  |SimpleBranch (ll, lw), CompoundBranch (rl, rm) -> lw + (sumweight rm)
  |CompoundBranch (ll, lm), CompoundBranch (rl, rm) -> (sumweight lm) +  (sumweight rm)

  let toque : branch -> int
  =fun b -> match b with
  |SimpleBranch (l, w) -> l * w
  |CompoundBranch (l, m) -> l * sumweight (m)

  let balsub : mobile -> bool
  =fun (lb, rb) -> toque lb = toque rb

  let rec  balanced : mobile -> bool
  =fun (lb,rb) -> match lb, rb with
  |SimpleBranch (ll, lw), SimpleBranch (rl, rw) -> balsub (lb, rb)
  |CompoundBranch (ll, lm), SimpleBranch (rl, rw) -> (balanced lm)&&(balsub (lb, rb))
  |SimpleBranch (ll, lw), CompoundBranch (rl, rm) -> (balanced rm)&&(balsub (lb, rb))
  |CompoundBranch (ll, lm), CompoundBranch (rl, rm) -> (balanced lm)&&(balanced rm)&&(balsub (lb, rb))
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

	let rec checkbound : exp -> var list -> bool 
	=fun e l -> match e with
	| V v -> List.mem v l
	| P (v,e1) ->	checkbound e1 (v::l)
	| C (e1,e2) -> (checkbound e1 l)&&(checkbound e2 l)
	
	let rec check : exp -> bool 
	=fun e -> checkbound e []
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
	=fun exp env -> match exp with 
    | CONST n -> Int n
    | VAR x -> apply_env env x
    | ADD (e1, e2) ->  
			let v1 = eval e1 env in
			let v2 = eval e2 env in
				(match v1, v2 with
				|	Int n1, Int n2 -> Int (n1 + n2)
				|	_ -> raise (Failure "Type Error: non-numeric values"))
		| SUB (e1, e2) ->  
			let v1 = eval e1 env in
			let v2 = eval e2 env in
				(match v1, v2 with
				|	Int n1, Int n2 -> Int (n1 - n2)
				|	_ -> raise (Failure "Type Error: non-numeric values")) 
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
    | LETREC (f, x, e1, e2) -> eval e2 (extend_env (f, RecProcedure (f, x, e1, env)) env)
    | PROC (x, e) -> Procedure (x, e, env)
    | CALL (e1, e2) -> 
			(match eval e1 env with
			|	Procedure (x, e, env1) ->
				let v = eval e2 env in
				eval e (extend_env (x,v) env1)
			|	RecProcedure (f, x, e, env1) ->
				let v = eval e2 env in
				eval e (extend_env (x, v) (extend_env (f, RecProcedure(f, x, e, env1)) env1))
			| _ -> raise (Failure "Type Error: former expression must be Procedure or RecProcedure type"))
 
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

	type env = var list  

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
	
	let rec lex_dpth : var -> env -> int
	=fun var env -> match env with
		|	hd::tl -> if var = hd then 0 else 1 + lex_dpth var tl
		|	[] -> raise (Failure "Environment is empty")

	let rec transexp : exp -> env -> nl_exp
	=fun exp env -> match exp with
		| CONST int -> NL_CONST int
    | VAR x -> NL_VAR (lex_dpth x env)
    | ADD (e1, e2) -> NL_ADD (transexp e1 env, transexp e2 env)  
    | SUB (e1, e2) -> NL_SUB (transexp e1 env, transexp e2 env)
    | ISZERO e -> NL_ISZERO (transexp e env)
    | IF (e1, e2, e3) -> NL_IF (transexp e1 env, transexp e2 env, transexp e3 env)
    | LET (x, e1, e2) -> let env1 = x::env in NL_LET (transexp e1 env, transexp e2 env1)
    | PROC (x, e) -> let env1 = x::env in NL_PROC (transexp e env1)
    | CALL (e1, e2) -> NL_CALL (transexp e1 env, transexp e2 env)

  let translate : program -> nl_program
  =fun pgm -> transexp pgm []
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
  
	let rec eval : nl_exp -> nl_env -> nl_value
	=fun exp env -> match exp with 
    | NL_CONST n -> NL_Int n
    | NL_VAR n -> List.nth env n
    | NL_ADD (e1, e2) ->  
			let v1 = eval e1 env in
			let v2 = eval e2 env in
				(match v1, v2 with
				|	NL_Int n1, NL_Int n2 -> NL_Int (n1 + n2)
				|	_ -> raise (Failure "Type Error: non-numeric values"))
		| NL_SUB (e1, e2) ->  
			let v1 = eval e1 env in
			let v2 = eval e2 env in
				(match v1, v2 with
				|	NL_Int n1, NL_Int n2 -> NL_Int (n1 - n2)
				|	_ -> raise (Failure "Type Error: non-numeric values")) 
		| NL_ISZERO e ->
			(match eval e env with
			| NL_Int n when n = 0 -> NL_Bool true
			| _ -> NL_Bool false)
		| NL_IF (e1,e2,e3) ->
			(match eval e1 env with
			| NL_Bool true -> eval e2 env
			| NL_Bool false -> eval e3 env
			| _ -> raise (Failure "Type Error: condition must be NL_Bool type"))
		| NL_LET (e1,e2) ->
				let v1 = eval e1 env in
				eval e2 (v1::env)
    | NL_PROC e -> NL_Procedure (e, env)
    | NL_CALL (e1, e2) -> 
			(match eval e1 env with
			|	NL_Procedure (e, env1) ->
				let v = eval e2 env in
				eval e (v::env1)
			| _ -> raise (Failure "Type Error: former expression must be NL_Procedure type"))
 
   let nl_run : nl_program -> nl_value
  =fun pgm -> eval pgm []
end

