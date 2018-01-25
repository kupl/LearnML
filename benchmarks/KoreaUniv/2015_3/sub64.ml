
(* 1. you can modify the given function specifications as recursive. *)
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

let rec calpweight br =
		match br with
		SimpleBranch (l,w) -> w |
		CompoundBranch (l,m) ->
				(match m with
				(a,b) -> (calpweight a) + (calpweight b));;

let rec calweight br =
		match br with
		SimpleBranch (l,w) -> l*w |
		CompoundBranch (l,m) ->
				(match m with
				(a,b) -> if (calweight a)=(calweight b) then (((calpweight a)+(calpweight b))*l) else (-1));;

let balanced : mobile -> bool
=fun (lb,rb) ->
		 if ((calweight lb)=(calweight rb)) then true else false;;

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let rec vpc_eval exp1 ar_env =
		match exp1 with
		V a ->
				let rec ck l1 st =
					match l1 with
					[] -> false |
					h::t -> if h=st then true else (ck t st) in
				if (ck ar_env a) then true else false |
		P (v, e1) -> if (vpc_eval e1 (ar_env@[v])) then true else false |
		C (e1, e2) -> if (vpc_eval e1 ar_env && vpc_eval e2 ar_env) then true else false;;

let check : exp -> bool
=fun e ->
			vpc_eval e [];;

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

let rec eval exp env =
		match exp with
		CONST n -> Int n |
		VAR x -> apply_env env x |
		ADD (e1,e2) -> 
			let v1 = eval e1 env in let v2 = eval e2 env in
			(match (v1,v2) with
			(Int n1, Int n2) -> Int (n1+n2) |
			(_,_) -> raise (Failure "Type-Error")) |
		SUB (e1,e2) ->
			let v1 = eval e1 env in let v2 = eval e2 env in
			(match (v1,v2) with
			(Int n1, Int n2) -> Int (n1-n2) |
			(_,_) -> raise (Failure "Type-Error")) |
		ISZERO e ->
			(match (eval e env) with
			Int n when n=0 -> Bool true |
			_ -> Bool false) |
		IF (e1,e2,e3) ->
			(match (eval e1 env) with
			Bool true -> eval e2 env |
			Bool false -> eval e3 env |
			_ -> raise (Failure "Type-Error")) |
		LET (x,e1,e2) ->
			let v1 = eval e1 env in
				eval e2 (extend_env (x,v1) env) |
		LETREC (st,x,e1,e2) ->
				eval e2 (extend_env (st, RecProcedure (st,x,e1,env)) env) |
		PROC (x,e) -> Procedure (x,e,env) |
		CALL (e1,e2) ->
			(match eval e1 env with
			Procedure (xp,ep,envp) -> eval ep (extend_env (xp, (eval e2 env)) envp) |
			RecProcedure (stp,xp,ep,envp) ->
					let env1 = extend_env (xp, (eval e2 env)) envp in
					let env2 = extend_env (stp, RecProcedure(stp,xp,ep,envp)) env1 in
					eval ep env2 |
			_ -> raise (Failure "Type-Error"));;

let run : program -> value
=fun pgm -> eval pgm empty_env;;

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


let rec nl_trans exp tr_env =
		match exp with
		CONST n -> NL_CONST n |
		VAR a ->
			let temp_num = ref 0 in
			let rec cornum envc =
				match envc with
				[] -> raise (Failure "Freevariable") |
				h::t -> if h=a then NL_VAR (!temp_num)
											else ((temp_num := (!temp_num)+1); (cornum t)) in
				cornum tr_env |
		ADD (e1,e2) -> NL_ADD (nl_trans e1 tr_env, nl_trans e2 tr_env) |
		SUB (e1,e2) -> NL_SUB (nl_trans e1 tr_env, nl_trans e2 tr_env) |
		ISZERO e -> NL_ISZERO (nl_trans e tr_env) |
		IF (e1,e2,e3) -> NL_IF (nl_trans e1 tr_env,nl_trans e2 tr_env,nl_trans e3 tr_env) |
		LET (v,e1,e2) -> NL_LET (nl_trans e1 tr_env, nl_trans e2 (v::tr_env)) |
		PROC (v,e) -> NL_PROC (nl_trans e (v::tr_env)) |
		CALL (e1,e2) -> NL_CALL (nl_trans e1 tr_env, nl_trans e2 tr_env);;

let translate : program -> nl_program
=fun pgm ->
		nl_trans pgm [];;

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

let rec nl_eval exp nl_env =
		match exp with
		NL_CONST n -> NL_Int n |
		NL_VAR n ->
			let rec findval nf envf =
				(match envf with
				[] -> raise (Failure "Freevariable") |
				h::t -> if nf=0 then h else findval (nf-1) t) in
				findval n nl_env |
		NL_ADD (e1,e2) ->
			let v1 = nl_eval e1 nl_env in let v2 = nl_eval e2 nl_env in
			(match (v1,v2) with
			(NL_Int n1, NL_Int n2) -> NL_Int (n1+n2) |
			_ -> raise (Failure "Type-Error")) |
		NL_SUB (e1,e2) ->
			let v1 = nl_eval e1 nl_env in let v2 = nl_eval e2 nl_env in
			(match (v1,v2) with
			(NL_Int n1, NL_Int n2) -> NL_Int (n1-n2) |
			_ -> raise (Failure "Type-Error")) |
		NL_ISZERO e ->
			(match (nl_eval e nl_env) with
			NL_Int n when n=0 -> NL_Bool true |
			_ ->  NL_Bool false) |
		NL_IF (e1,e2,e3) ->
			(match (nl_eval e1 nl_env) with
			NL_Bool true -> nl_eval e2 nl_env |
			NL_Bool false -> nl_eval e3 nl_env |
			_ -> raise (Failure "Type-Error")) |
		NL_LET (e1, e2) ->
			let v1 = nl_eval e1 nl_env in
			nl_eval e2 (v1::nl_env) |
		NL_PROC e -> NL_Procedure (e,nl_env) |
		NL_CALL (e1,e2) ->
			(match (nl_eval e1 nl_env) with
			NL_Procedure (ep,nl_envp) ->
					let v1 = nl_eval e2 nl_env in
					nl_eval ep (v1::nl_envp) |
			_ -> raise (Failure "Type-Error"));;

let nl_run : nl_program -> nl_value
=fun pgm ->
		nl_eval pgm [];;

end
