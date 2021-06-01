(* 2012-11230 Kim sangmin *)
type lambda = V of var
		   | P of var*lambda
		   | C of lambda*lambda
and var = string

let rec check : lambda -> bool = fun x ->
	let rec helper : lambda*string list -> bool = fun (met, env) ->
		match met with
		| V(n) -> if(List.mem n env) then true else false
		| P(n, m) -> if(List.mem n env) then helper(m,env) else helper(m, n::env)
		| C(m1, m2) -> if(helper(m1,env) && helper(m2,env)) then true else false
	in
	helper(x, [])


