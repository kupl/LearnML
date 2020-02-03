type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check m =
	let rec check2 m s = match m with
		| V n -> List.mem n s
		| P (n, m') -> check2 m' (n::s)
		| C (m1, m2) -> check2 m1 s && check2 m2 s
	in
	check2 m []
