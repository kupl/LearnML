type name = string

type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro

let rec is_exist (str, lst) =
	match (str, lst) with
	| (str, []) -> false
	| (str, h::t) -> if str=h then true
					 else is_exist (str, t)

let rec checkMetro mtr =
	let rec check_existing (mtr, lst) =
		match (mtr, lst) with
		| ((STATION n), lst) -> is_exist (n, lst)
		| ((AREA (n, m)), lst) -> check_existing (m, n::lst)
		| ((CONNECT (m1, m2)), lst) -> (check_existing (m1, lst)) && (check_existing (m2, lst))
	in
	check_existing (mtr, [])