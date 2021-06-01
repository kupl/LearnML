type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check2(lambda, lst) =
	match lambda with
    | V (var) -> 
			  List.mem var lst
		| C(lambda1,lambda2) ->
			(check2(lambda1, lst) && check2(lambda2, lst))
		| P(var, lambda) ->
			(check2(lambda, List.append [var] lst));;
			
let check (lambda) =
		check2(lambda, []);;

