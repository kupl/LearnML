type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec find_env : var -> var list -> bool
= fun var env ->
  match env with
  | [] -> false
  | hd::tl -> 
    if (hd = var) then true
    else find_env var tl

let rec lambda_env : lambda -> var list -> bool
= fun lam env ->
  match lam with 
  | V x ->
    if (find_env x env = false) then false
    else true
  | P (x, l1) ->
    lambda_env l1 (x::env)
  | C (l1, l2) ->
    let t1 = lambda_env l1 env in
    let t2 = lambda_env l2 env in
    if (t1 = true && t2 = true) then true
    else false
  ;;

let check : lambda -> bool
= fun lam -> lambda_env lam []
