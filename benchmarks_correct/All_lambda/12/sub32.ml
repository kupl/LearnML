type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec check m =
	let rec check2 l m =
		match m with
		| V n -> List.mem n l
		| P (n, m) -> check2 (n::l) m
		| C (m1, m2) -> (check2 l m1) && (check2 l m2)
	in
check [] m
