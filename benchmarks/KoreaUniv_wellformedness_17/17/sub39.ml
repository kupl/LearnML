(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type env2 = var list
let empty_env2 = []
let extend_env2 x e = x::e
let rec apply_env2 e x = 
  match e with
  | [] -> false
  | y::tl -> if x = y then true else apply_env2 tl x

let rec check : lambda -> bool
= fun lam ->  let rec help2 : lambda -> env2 -> bool
  = fun lam env -> match lam with
    | V x -> apply_env2 env x
    | P (x, lam2) -> let env2 = extend_env2 x env in
      (help2 lam2 env2)
    | C (lam1, lam2) -> (match help2 lam1 env with
      | true -> help2 lam2 env
      | false -> false)
  in help2 lam empty_env2