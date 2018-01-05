type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
	and name = string
let checkMetro map =
	let rec checkMetro' (arealist, map) = 
		match map with
		| STATION name -> if (List.mem name arealist) then true else false
		| CONNECT (map1, map2) -> checkMetro' (arealist, map1) && checkMetro' (arealist, map2)
		| AREA (name, map') -> checkMetro' (name::arealist, map')
	in
	checkMetro' ([], map)
