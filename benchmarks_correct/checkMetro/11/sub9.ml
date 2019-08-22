type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check : lambda -> bool = fun startm ->
	let rec check' : lambda * var list -> bool = fun (m, l) ->
		match m with
		| V n ->
			List.mem n l
		| P (n1, m2) ->
			if (List.mem n1 l) = false then check'(m2, n1::l)
			else check'(m2, l)
		| C (m1, m2) ->
			check'(m1, l) && check'(m2, l)
	in
		let startl : var list = []
		in
			check' (startm, startl)