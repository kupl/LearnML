(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec custom_check : lambda -> env1 -> bool
= fun lam env -> match lam with
	| V(x) -> (applyer_env env x)
	| P(x, l) -> custom_check l (extender_env (x, true) env)
	| C(l1, l2) -> if (custom_check l1 env) then (custom_check l2 env) else false

let rec check : lambda -> bool
= fun lam -> custom_check lam empty_env
