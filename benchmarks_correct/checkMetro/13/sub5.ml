type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check lambda =
	let rec check_sub lambda env =
		match lambda with
		| V var -> 
			(List.exists (fun x -> x = var) env)
		| P (var, m) ->
			(check_sub m (var::env))
		| C (m1, m2) ->
			(check_sub m1 env) && (check_sub m2 env)
	in
	(check_sub lambda [])
