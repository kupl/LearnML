(**********************)
(*    Problem 1       *)
(**********************)
type exp =
	| CONST of int
	| VAR of var
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| ISZERO of exp
	| READ
	| IF of exp * exp * exp
	| LET of var * exp * exp
	| LETREC of var * var * exp * exp
	| PROC of var * exp
	| CALL of exp * exp
and var = string

type env = (var * exp) list
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x =
	match e with
	| [] -> VAR x
	| (y,v)::tl -> if x=y then v else apply_env tl x

let rec check_env : exp -> var -> bool
= fun exp var -> match exp with
	| CONST n -> false
	| VAR x -> if x=var then true else false
	| ADD (e1,e2) -> (check_env e1 var) || (check_env e2 var)
	| SUB (e1,e2) -> (check_env e1 var) || (check_env e2 var)
	| MUL (e1,e2) -> (check_env e1 var) || (check_env e2 var)
	| DIV (e1,e2) -> (check_env e1 var) || (check_env e2 var)
	| ISZERO e -> check_env e var
	| READ -> false
	| IF (e1,e2,e3) -> (check_env e1 var) || (check_env e2 var) || (check_env e3 var)
	| LET (v,e1,e2) -> (check_env e2 var) || (check_env e1 var)
	| LETREC (v1,v2,e1,e2) -> (check_env e2 var) || (check_env e1 var)
	| PROC (v,e) -> check_env e var
	| CALL (e1,e2) -> (check_env e1 var) || (check_env e2 var) 

let rec expand_env : exp -> env -> exp
= fun exp env -> match exp with
	| CONST n -> CONST n
	| VAR x -> apply_env env x
	| ADD (e1,e2) -> ADD (expand_env e1 env,expand_env e2 env)
	| SUB (e1,e2) -> SUB (expand_env e1 env,expand_env e2 env)
	| MUL (e1,e2) -> MUL (expand_env e1 env,expand_env e2 env)
	| DIV (e1,e2) -> DIV (expand_env e1 env,expand_env e2 env)
	| ISZERO e -> ISZERO (expand_env e env)
	| READ -> CONST (read_int())
	| IF (e1,e2,e3) -> IF (expand_env e1 env,expand_env e2 env,expand_env e3 env)
	| LET (v,e1,e2) -> if check_env e2 v then	let env1 = extend_env (v,expand_env e1 env) env in expand_env e2 env1 else exp
	| LETREC (v1,v2,e1,e2) -> LETREC (v1,v2,expand_env e1 env,expand_env e2 env)
	| PROC (v,e) -> PROC (v,expand_env e env)
	| CALL (e1,e2) -> CALL (expand_env e1 env,expand_env e2 env)
let rec expand : exp -> exp
= fun exp -> expand_env exp empty_env

(**********************)
(*    Problem2        *)
(**********************)

type lambda = V of var
						| P of var * lambda
						| C of lambda * lambda
and var = string

type env = (var) list
let lambda_env = []
let extend_str x e = x::e
let rec inplace_env e x =
	match e with
	| [] -> false
	| y::tl -> if x=y then true else inplace_env tl x

let rec check_lambda : lambda -> env -> bool
= fun lam env -> match lam with
	| V s -> inplace_env env s
	| P (s,l) -> let env1=extend_str s env in check_lambda l env1
	| C (l1,l2) -> (check_lambda l1 env) && (check_lambda l2 env)

let rec check : lambda -> bool
= fun lam -> check_lambda lam lambda_env
