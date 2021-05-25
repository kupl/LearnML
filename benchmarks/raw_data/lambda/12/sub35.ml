type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
	and var = string

let rec check m =
	let is_in lst sta =
		List.mem sta lst
	in
	let rec check2 m lst =
		match (m,lst) with
		|(V a,lst) -> (is_in lst a)
		|(P (a,b),lst) -> (check2 b (a::lst))
		|(C (a,b),lst) -> (check2 a lst) && (check2 b lst)
	in
	check2 m []
