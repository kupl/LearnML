type lambda =
	  V of var
	| P of var * lambda
	| C of lambda * lambda

and var = string

let check met =
	let rec checkWithList (m, l) = match m with
		| V n -> List.mem n l
		| P (n, m') -> checkWithList (m', n::l)
		| C (m1, m2) -> checkWithList (m1, l) && checkWithList (m2, l)
	in
	checkWithList (met, [])