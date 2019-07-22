type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec checkMetro m =
	let rec check l m =
		match m with
		| STATION n -> List.mem n l
		| AREA (n, m) -> check (n::l) m
		| CONNECT (m1, m2) -> (check l m1) && (check l m2)
	in
check [] m
