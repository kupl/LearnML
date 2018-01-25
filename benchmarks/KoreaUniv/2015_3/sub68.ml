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
  
  let rec branchweight br =
  	let mobileweight = fun (lb,rb) -> branchweight(lb)+branchweight(rb) in
  	match br with
  	| SimpleBranch(le,we) -> we
  	| CompoundBranch(le, mo) -> mobileweight(mo)

  let mobileweight = fun (lb,rb) -> branchweight(lb)+branchweight(rb)
  
  let branchbalance br = 
  	match br with
  	| SimpleBranch(le,we) -> le*we
  	| CompoundBranch(le,mo) -> le*mobileweight(mo)

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> if (branchbalance lb)=(branchbalance rb) then
  	(match lb with
  	| SimpleBranch(le,we) ->
  		(match rb with
  		| SimpleBranch(le2,we2) -> true
  		| CompoundBranch(le2, mo) -> balanced mo)
  	| CompoundBranch(le,mo) ->
  		(match rb with
  		| SimpleBranch(le2,we) -> balanced mo
  		| CompoundBranch(le2,mo2) -> if balanced mo then balanced mo2 else false))
  else false
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
  =fun e ->
    let rec checkp = fun (va,ex) -> 
	match ex with
  	| V(va2) -> if va=va2 then true else false
  	| P(va2,ex2) ->
  		(match ex2 with
  		| C(ex3,ex4) -> if (checkp(va,ex3)||checkp(va2,ex3))&&(checkp(va,ex4)||checkp(va2,ex4)) then true else false
  		| _ -> if checkp(va,ex2)||checkp(va2,ex2) then true else false)
  	| C(ex2,ex3) -> if checkp(va,ex2)&&checkp(va,ex3) then true else false in
  match e with
  | V(va) -> false
  | P(va,ex) -> checkp(va,ex)
  | C(ex,ex2) -> if check(ex)&&check(ex2) then true else false
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
			(match v1,v2 with
			| Int n1, Int n2 -> Int (n1 + n2)
			| _ -> raise (Failure "Type Error: non-numeric values"))
	| SUB (e1,e2) ->
		let v1 = eval e1 env in
		let v2 = eval e2 env in
			(match v1,v2 with
			| Int n1, Int n2 -> Int (n1 - n2)
			| _ -> raise (Failure "Type Error: non-numeric values"))
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
	| LETREC (f,x,e1,e2) -> eval e2 (extend_env (f,RecProcedure(f,x,e1,env)) env)
	| PROC (x,e) -> Procedure(x,e,env)
	| CALL (e1,e2) ->
		match (eval e1 env) with
		| Procedure (x,e,p) -> eval e (extend_env (x,eval e2 env) p)
		| RecProcedure (f,x,e,p) -> eval e (extend_env (x,eval e2 env) (extend_env (f,RecProcedure(f,x,e,p)) p))

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

  let rec lc
  =fun l v index ->
  match l with
  | [] -> raise (Failure "not!")
  | hd::tl -> if (hd=v) then index else lc tl v index+1

  let rec lex : exp -> string list -> nl_exp
  =fun exp envl ->
  	match exp with
	| CONST n -> NL_CONST n
	| VAR x -> NL_VAR (lc envl x 0)
	| ADD (e1,e2) ->
		let v1 = lex e1 envl in
		let v2 = lex e2 envl in
			NL_ADD(v1,v2)
	| SUB (e1,e2) ->
		let v1 = lex e1 envl in
		let v2 = lex e2 envl in
		NL_SUB(v1,v2)
	| ISZERO e -> NL_ISZERO(lex e envl)
	| IF (e1,e2,e3) -> NL_IF(lex e1 envl,lex e2 envl,lex e3 envl)
	| LET (x,e1,e2) -> NL_LET(lex e1 envl, lex e2 (x::envl))
	| PROC (x,e) -> NL_PROC (lex e (x::envl))
	| CALL (e1,e2) -> NL_CALL (lex e1 envl, lex e2 envl)

  let translate : program -> nl_program
  =fun pgm -> lex pgm []

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
  
  let rec nl_eval : nl_exp -> nl_env -> nl_value
  =fun exp env ->
	match exp with
	| NL_CONST n -> NL_Int n
	| NL_ADD (e1,e2) ->
		let v1 = nl_eval e1 env in
		let v2 = nl_eval e2 env in
			(match v1,v2 with
			| NL_Int n1, NL_Int n2 -> NL_Int (n1 + n2)
			| _ -> raise (Failure "Type Error: non-numeric values"))
	| NL_VAR n -> List.nth env n
	| NL_SUB (e1,e2) ->
		let v1 = nl_eval e1 env in
		let v2 = nl_eval e2 env in
			(match v1,v2 with
			| NL_Int n1, NL_Int n2 -> NL_Int (n1 - n2)
			| _ -> raise (Failure "Type Error: non-numeric values"))
	| NL_ISZERO e ->
		(match nl_eval e env with
		| NL_Int n when n = 0 -> NL_Bool true
		| _ -> NL_Bool false)
	| NL_IF (e1,e2,e3) ->
		(match nl_eval e1 env with
		| NL_Bool true -> nl_eval e2 env
		| NL_Bool false -> nl_eval e3 env
		| _ -> raise (Failure "Type Error: condition must be Bool type"))
	| NL_LET (e1,e2) ->
		let v1 = nl_eval e1 env in
		nl_eval e2 (v1::env)
	| NL_PROC (e) -> NL_Procedure(e,env)
	| NL_CALL (e1,e2) ->
		match (nl_eval e1 env) with
		| NL_Procedure (e,p) -> nl_eval e ((nl_eval e2 env)::p)

  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm []
end
