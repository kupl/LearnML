type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string


let rec check (area, m) =
	match m with
	| V _var -> List.mem _var area
	| P (_var, _lambda) -> check (_var::area, _lambda)
	| C (_lambda1, _lambda2) -> check (area, _lambda1) && check (area, _lambda2)

let rec check m =
	match m with
	| V _var -> false
	| P (_var, _lambda) -> check (_var::[], _lambda)
	| C (_lambda1, _lambda2) -> check _lambda1 && check _lambda2







