type lambda =
	V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec check lambda =
	let rec check lambda area =
		match lambda with
		V station -> List.mem station area
		| P (var, lambda) -> check lambda (var::area)
		| C (lambda1, lambda2) -> (check lambda1 area) && (check lambda2 area)
	in
	check lambda []
	
