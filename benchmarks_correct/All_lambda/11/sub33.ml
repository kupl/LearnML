type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check m =
	let rec check m s = match m with
		| V n -> List.mem n s
		| P (n, m') -> check m' (n::s)
		| C (m1, m2) -> check m1 s && check m2 s
	in
	check m []
