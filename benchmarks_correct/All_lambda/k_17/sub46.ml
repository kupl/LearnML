(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
and env = var list

let empty_env = []
let extend_env v e = v::e
let rec apply_env e x = 
  match e with
  | [] -> false
  | y::tl -> if x = y then true else apply_env tl x

let rec isclosed : lambda -> env -> bool
=fun lam curr_env ->
  match lam with
  V v -> apply_env curr_env v
| P (v,e) -> isclosed e (extend_env v curr_env)
| C (e1,e2) -> (
    let b1 = isclosed e1 curr_env in
    let b2 = isclosed e2 curr_env in
    if b1 && b2 then true else false
)

let rec check : lambda -> bool
= fun lam -> isclosed lam empty_env