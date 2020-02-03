type lambda =
	V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec check lambda =
	let rec check2 lambda area =
		match lambda with
		V station -> List.mem station area
		| P (var, lambda) -> check2 lambda (var::area)
		| C (lambda1, lambda2) -> (check2 lambda1 area) && (check2 lambda2 area)
	in
	check2 lambda []
	
