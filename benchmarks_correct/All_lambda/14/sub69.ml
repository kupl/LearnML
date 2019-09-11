type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check(lambda, lst) =
	match lambda with
    | V (var) -> 
			  List.mem var lst
		| C(lambda1,lambda2) ->
			(check(lambda1, lst) && check(lambda2, lst))
		| P(var, lambda) ->
			(check(lambda, List.append [var] lst));;
			
let check (lambda) =
		check(lambda, []);;

