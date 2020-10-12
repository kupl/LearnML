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
