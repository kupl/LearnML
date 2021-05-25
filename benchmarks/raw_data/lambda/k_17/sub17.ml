(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string


let bind : var list -> var -> var list
= fun env v -> v::env

let lookup : var list -> var -> bool
= fun env v -> List.mem v env


let rec check_with_env : lambda -> var list -> bool
= fun lambda env -> match lambda with
 | V v -> lookup env v
 | P (v,e) -> let new_env = bind env v in
   check_with_env e new_env
 | C (e1,e2) -> check_with_env e1 env && check_with_env e2 env

let rec check : lambda -> bool
= fun lam -> check_with_env lam []

