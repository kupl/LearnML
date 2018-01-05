type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
	and name = string

let rec checkMetro m =
	let is_in lst sta =
		List.mem sta lst
	in
	let rec check m lst =
		match (m,lst) with
		|(STATION a,lst) -> (is_in lst a)
		|(AREA (a,b),lst) -> (check b (a::lst))
		|(CONNECT (a,b),lst) -> (check a lst) && (check b lst)
	in
	check m []
