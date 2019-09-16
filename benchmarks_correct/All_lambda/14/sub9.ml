type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string
(*
 * manually edited
 * checkMetro -> check
 * check -> check_sub
 *)

let rec check_sub (area, m) =
	match m with
	| V _var -> List.mem _var area
	| P (_var, _lambda) -> check_sub (_var::area, _lambda)
	| C (_lambda1, _lambda2) -> check_sub (area, _lambda1) && check_sub (area, _lambda2)

let rec check m =
	match m with
	| V _var -> false
	| P (_var, _lambda) -> check_sub (_var::[], _lambda)
	| C (_lambda1, _lambda2) -> check _lambda1 && check _lambda2







