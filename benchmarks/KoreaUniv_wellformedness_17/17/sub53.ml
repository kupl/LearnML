(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type venv = var list
let venv_empty = []

let expand_venv x env = x::env
let rec find_venv x env
= match env with
  | [] -> false
  | y::tl -> if x = y then true else find_venv x tl

let rec check_aux : lambda -> venv -> bool
= fun lam env ->
  match lam with
  | V x -> find_venv x env
  | P (x, l) -> check_aux l (expand_venv x env)
  | C (l1, l2) -> (check_aux l1 env) && (check_aux l2 env)
let check : lambda -> bool
= fun lam -> check_aux lam venv_empty
