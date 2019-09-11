type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check m =
	let rec _check(m,al) = match m with
		V(var) -> List.mem var al
		| P(var,m') -> _check(m',var::al)
		| C(m1,m2) -> _check(m1,al) && _check(m2,al)
	in _check(m,[])
