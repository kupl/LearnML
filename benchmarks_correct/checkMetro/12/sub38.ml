type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let rec checkMetro m =
	let rec check m area =
		match m with
		STATION(name) -> List.mem name area
		| AREA(name, m1) -> check m1 (name::area)
		| CONNECT(m1, m2) -> (check m1 area) && (check m2 area)
	in
	check m []
