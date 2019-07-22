type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec checkMetro metro = 
	let rec cmModule metro2 stations =
		match metro2 with
		| STATION name2 -> List.mem name2 stations
		| AREA (name2, metro3) -> cmModule metro3 (name2::stations)
		| CONNECT (metro3, metro4) -> (cmModule metro3 stations) && (cmModule metro4 stations)
		in
	cmModule metro [];;
