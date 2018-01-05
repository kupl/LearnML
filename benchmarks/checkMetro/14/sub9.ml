type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string


let rec check (area, m) =
	match m with
	| STATION _name -> List.mem _name area
	| AREA (_name, _metro) -> check (_name::area, _metro)
	| CONNECT (_metro1, _metro2) -> check (area, _metro1) && check (area, _metro2)

let rec checkMetro m =
	match m with
	| STATION _name -> false
	| AREA (_name, _metro) -> check (_name::[], _metro)
	| CONNECT (_metro1, _metro2) -> checkMetro _metro1 && checkMetro _metro2







