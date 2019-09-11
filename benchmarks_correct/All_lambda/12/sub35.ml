type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
	and var = string

let rec check m =
	let is_in lst sta =
		List.mem sta lst
	in
	let rec check m lst =
		match (m,lst) with
		|(V a,lst) -> (is_in lst a)
		|(P (a,b),lst) -> (check b (a::lst))
		|(C (a,b),lst) -> (check a lst) && (check b lst)
	in
	check m []
