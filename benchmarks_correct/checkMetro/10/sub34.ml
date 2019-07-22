type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let checkMetro m =
	let rec checkMetroRec avail m =
		match m with
			STATION s -> List.mem s avail
			| AREA (n,m1) -> checkMetroRec (n::avail) m1
			| CONNECT (m1,m2) -> (checkMetroRec avail m1) && (checkMetroRec avail m2)
	in
		checkMetroRec [] m