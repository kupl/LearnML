type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro m =
	let rec check m s = match m with
		| STATION n -> List.mem n s
		| AREA (n, m') -> check m' (n::s)
		| CONNECT (m1, m2) -> check m1 s && check m2 s
	in
	check m []
