(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
  let extend v env = v :: env in
  let find v env = List.mem v env in
  let rec check_env lambda env =
    match lambda with
    | V v -> find v env
    | P (v, l) -> let newenv = extend v env in
                  check_env l newenv
    | C (l1, l2) -> check_env l1 env && check_env l2 env

  in check_env lam []