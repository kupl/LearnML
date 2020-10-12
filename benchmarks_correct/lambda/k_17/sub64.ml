(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let empty_env_2 = fun _ -> false
let extend_env_2 v env = fun x -> if x = v then true else env x
let apply_env_2 x env = env x

let rec check_with_env lam env =
  match lam with
  | V x -> apply_env_2 x env

  | P (x, e) -> check_with_env e (extend_env_2 x env)

  | C (e1, e2) -> (check_with_env e1 env) && (check_with_env e2 env)

let rec check : lambda -> bool
= fun lam -> check_with_env lam empty_env_2