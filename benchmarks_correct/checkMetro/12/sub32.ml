type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec check m =
	let rec check l m =
		match m with
		| V n -> List.mem n l
		| P (n, m) -> check (n::l) m
		| C (m1, m2) -> (check l m1) && (check l m2)
	in
check [] m
