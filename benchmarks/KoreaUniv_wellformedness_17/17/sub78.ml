(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type lambda_env = var list

let rec extend_lenv x lenv = x :: lenv

let rec l_check : lambda -> lambda_env -> bool
= fun lam lenv ->
match lam with
| V x -> 
	(match lenv with
	|[] -> false
	|hd::tl -> if x = hd then true else l_check lam tl)
| P (x, lam) -> 
	let lenv = extend_lenv x lenv in
		l_check lam lenv
| C (l1, l2) ->	
	(l_check l1 lenv) && (l_check l2 lenv)



let rec check : lambda -> bool
= fun lam -> l_check lam []


