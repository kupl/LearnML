(**********************)
(*   Problem 2        *)
(**********************)

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
