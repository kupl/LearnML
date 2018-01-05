type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string

let rec checkMetro_ namelist m =
	match m with
	| STATION(id) -> (List.mem id namelist)
	| AREA(id, m) -> (checkMetro_ (id::namelist) m)
	| CONNECT(m1, m2) -> (checkMetro_ namelist m1 && checkMetro_ namelist m2)
	
let checkMetro m = (checkMetro_ [] m)
