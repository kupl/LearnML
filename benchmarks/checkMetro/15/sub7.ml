type metro = 	STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro
and name = string

let rec station_list x =
	match x with
	| STATION n -> [n]
	| AREA (n, m) -> (station_list m)
	| CONNECT (m1, m2) -> List.append (station_list m1) (station_list m2)

let rec checkMetro (x: metro): bool =
	match x with
	| STATION n -> false
	| AREA (n, m) -> List.mem n (station_list m)
	| CONNECT (m1, m2) -> false
