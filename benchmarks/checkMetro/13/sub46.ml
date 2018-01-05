type metro = STATION of name
					 | AREA of name * metro
					 | CONNECT of metro * metro
and name = string

let checkMetro metro =
	let rec check m =
		match m with
		| STATION x -> [x]
		| AREA (x, y) -> List.filter (fun n -> not (x = n)) (check y)
		| CONNECT (x, y) -> (check x) @ (check y)
	in List.length (check metro) = 0
