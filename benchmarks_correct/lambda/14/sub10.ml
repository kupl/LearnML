type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec exists (l, a) = 
        match l with
        | [] -> false
        | h::t -> exists (t, a) || (a = h)

let rec check2 a lambda = 
	match lambda with
	| V var -> exists (a, var)
	| P (var, lambda) -> check2 (var::a) lambda
	| C (lambda1, lambda2) -> check2 a lambda1 && check2 a lambda2

let check = fun a ->
         check2 [] a;;