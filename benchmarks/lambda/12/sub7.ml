type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check m =
	let rec check2(m,al) = match m with
		V(var) -> List.mem var al
		| P(var,m') -> check2(m',var::al)
		| C(m1,m2) -> check2(m1,al) && check2(m2,al)
	in check2(m,[])
