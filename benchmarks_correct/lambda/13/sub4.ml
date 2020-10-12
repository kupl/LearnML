type var = string

type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda

let rec is_exist (str, lst) =
	match (str, lst) with
	| (str, []) -> false
	| (str, h::t) -> if str=h then true
					 else is_exist (str, t)

let rec check mtr =
	let rec check_existing (mtr, lst) =
		match (mtr, lst) with
		| ((V n), lst) -> is_exist (n, lst)
		| ((P (n, m)), lst) -> check_existing (m, n::lst)
		| ((C (m1, m2)), lst) -> (check_existing (m1, lst)) && (check_existing (m2, lst))
	in
	check_existing (mtr, [])