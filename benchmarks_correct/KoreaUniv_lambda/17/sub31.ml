(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type env = var list

let empty_env = []
let extend_env x lst = x::lst
let rec apply_env lst x = 
  match lst with
  | [] -> false
  | (y)::tl -> if x = y then true else apply_env tl x


let rec check_env : lambda -> env -> bool
= fun lam env -> match lam with
                | V str1 -> apply_env env str1
                | P (str1, lam1) -> check_env lam1 (extend_env str1 env)
                | C (lam1, lam2) -> (check_env lam1 env)&&(check_env lam2 env)


let rec check : lambda -> bool
= fun lam -> check_env lam empty_env
