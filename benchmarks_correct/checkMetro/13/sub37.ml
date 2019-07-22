type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec check namelist metro =
			match metro with
			| (STATION n) -> if (List.mem n namelist) then true else false
			| (AREA (n, m)) -> (check (List.append [n] namelist) m)
			| (CONNECT (m1, m2)) -> (check namelist m1) && (check namelist m2)

let rec checkMetro metro =
			match metro with
			| (STATION n) -> false
			| (AREA (n, m)) -> check [n] m
			| (CONNECT (m1, m2)) -> (checkMetro m1) && (checkMetro m2)
