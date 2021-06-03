type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let rec check m =
	let rec check2 m area =
		match m with
		V(var) -> List.mem var area
		| P(var, m1) -> check2 m1 (var::area)
		| C(m1, m2) -> (check2 m1 area) && (check2 m2 area)
	in
	check2 m []
