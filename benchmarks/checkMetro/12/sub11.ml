type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro and name = string


let rec checkMetro m =
	let rec check m a = 
		match m with
		| STATION n -> List.mem n a
		| AREA(n, m1) -> check m1 (n::a)
		| CONNECT(m1, m2) -> check m1 a && check m2 a
	in check m []
