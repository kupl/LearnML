type name =
string

type metro =
  STATION of name
| AREA of name * metro
| CONNECT of metro * metro

let checkMetro m =
	let rec checkMetro1 m city =
	match m with
	  STATION n -> List.mem n city
	| AREA (n, m1) -> let city1 = n::city in checkMetro1 m1 city1
	| CONNECT (m1, m2) -> checkMetro1 m1 city && checkMetro1 m2 city
	in
checkMetro1 m []
