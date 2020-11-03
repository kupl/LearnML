type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check lambda =
	let rec isThereID lambda l =
		match lambda with
		| V n -> List.mem n l
		| P (n, m) -> isThereID m (n::l)
		| C (m1, m2) -> (isThereID m1 l && isThereID m2 l)
	in
	isThereID lambda []