(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let empty_venv = []
let extend_venv x e = x::e
let rec venv_contains e x =
  match e with
  | [] -> false
  | hd::tl -> if hd = x then true else venv_contains tl x

type venv = string list

let rec check_with_venv : lambda -> venv -> bool
= fun lam venv->
  match lam with
  | V x -> (venv_contains venv x)
  | P (x,l) -> let ext_venv = extend_venv x venv in
               (check_with_venv l ext_venv)
  | C (l1,l2) -> (check_with_venv l1 venv) && (check_with_venv l2 venv)

let rec check : lambda -> bool
= fun lam -> 
  check_with_venv lam (empty_venv)
