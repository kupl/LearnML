(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
type var_env = string list

let rec isbound: var_env -> string -> bool
= fun env str ->
	match env with
	|[] -> false
	|hd::tl -> if hd = str then true else isbound tl str

let rec check_lam
= fun env lam ->
	match lam with
	| V x -> if isbound env x then true else false
	| P (x, e) -> let nenv = x::env in check_lam nenv e
	| C (e1, e2) -> check_lam env e1 && check_lam env e2

let rec check : lambda -> bool
= fun lam -> check_lam [] lam
