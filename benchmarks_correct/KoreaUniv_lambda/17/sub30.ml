(**********************)
(*   Problem 2        *)
(**********************)
let empty_env2 = []
let extend_env2 x e = x::e
let rec apply_env2 e x =
   match e with
    | [] -> false
    | y::tl -> if x = y then true else apply_env2 tl x

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec help_check lam env 
              = match lam with
                 | V(x) -> if (apply_env2 env x) == true then true
                           else false
                 | P(x,y) -> (help_check y (extend_env2 x env))
                 | C(x,y) -> (help_check x env)&&(help_check y env)
              in (help_check lam empty_env2)
