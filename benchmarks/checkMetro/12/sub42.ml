type metro =
	STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec checkMetro metro =
	let rec check metro area =
		match metro with
		STATION station -> List.mem station area
		| AREA (name, metro) -> check metro (name::area)
		| CONNECT (metro1, metro2) -> (check metro1 area) && (check metro2 area)
	in
	check metro []
	
