type lambda = V of var
			|P of var * lambda
			|C of lambda * lambda
and var = string

let rec check a =
	let rec cm l a =
		match a with
		V(var) -> List.mem var l
		|P(var, lambda) -> cm (List.append l [var]) lambda
		|C(lambda, lambda2) -> (cm l lambda) && (cm l lambda2) in
	cm [] a
