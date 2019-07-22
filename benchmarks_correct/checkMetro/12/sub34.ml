type metro = STATION of name
		   | AREA of name * metro
   		   | CONNECT of metro * metro
and name = string


let rec checkMetro metro = 
	let rec checkiter (metro, arealist) =
		match (metro, arealist) with
			| (STATION name, arealist) -> List.mem name arealist
			| (AREA (areaname, next), arealist) -> checkiter (next, areaname::arealist)
			| (CONNECT (metro1, metro2), arealist) -> (match (checkiter (metro1, arealist), checkiter (metro2, arealist)) with
												| (true,true) -> true
												| _ -> false
												)

	in checkiter (metro, [])
