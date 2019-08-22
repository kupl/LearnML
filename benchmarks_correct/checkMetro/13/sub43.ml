type lambda = V of var
					| P of var * lambda
					| C of lambda * lambda
	and var = string

let check = fun mm ->
	let rec checkL = fun mmm l ->
		match mmm with
			V(var) -> (List.mem var l)
			| P(var,m) -> (checkL m (l@[var]))
			| C(m1, m2) -> ((checkL m1 l) && (checkL m2 l))
	in checkL mm []
