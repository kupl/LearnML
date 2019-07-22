(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam -> let env = [] in
let extend_env v env = v::env in
let rec find_env v env = match env with
  | [] -> false
  | hd::tl -> if hd=v then true else find_env v tl in
let rec eval = fun lam env -> match lam with
  | V v -> find_env v env
  | P (v, l) -> (let env' = extend_env v env in
      eval l env')
  | C (l1, l2) -> (eval l1 env) && (eval l2 env) in
  eval lam env
