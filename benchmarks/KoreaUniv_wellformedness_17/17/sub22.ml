(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

(*added function. returns true if given variable is bound, false otherwise*)
let rec apply = fun var env -> match env with
                               | h::t -> if var=h then true else apply var t
                               | [] -> false

(*added function. checks all variables in the body of the given lambda expression whether it is bound or not*)
let rec bound_var = fun l env -> match l with
                                 | V var -> apply var env
                                 | P (var, lambda) -> bound_var lambda (var::env)
                                 | C (lambda1, lambda2) -> (bound_var lambda1 env) && (bound_var lambda2 env)

let rec check : lambda -> bool
= fun lam -> bound_var lam []
