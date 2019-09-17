type lambda = V of var | P of var * lambda | C of lambda * lambda and var = string


let rec check m =
	let rec check m a = 
		match m with
		| V n -> List.mem n a
		| P(n, m1) -> check m1 (n::a)
		| C(m1, m2) -> check m1 a && check m2 a
	in check m []
