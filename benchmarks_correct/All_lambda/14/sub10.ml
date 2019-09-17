type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec exists (l, a) = 
        match l with
        | [] -> false
        | h::t -> exists (t, a) || (a = h)

let rec _check a lambda = 
	match lambda with
	| V var -> exists (a, var)
	| P (var, lambda) -> _check (var::a) lambda
	| C (lambda1, lambda2) -> _check a lambda1 && _check a lambda2

let check = fun a ->
         _check [] a;;