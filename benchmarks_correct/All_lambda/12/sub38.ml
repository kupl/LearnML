type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let rec check m =
	let rec check m area =
		match m with
		V(var) -> List.mem var area
		| P(var, m1) -> check m1 (var::area)
		| C(m1, m2) -> (check m1 area) && (check m2 area)
	in
	check m []
