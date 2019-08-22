type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec check m =
	let rec checkArea (m, nl) =
		match m with
		| V n -> (List.mem n nl)
		| P (n, m) -> (checkArea (m, (n::nl)))
		| C (m1, m2) -> (checkArea (m1, nl)) && (checkArea (m2, nl))
	in

	(checkArea (m, []))
