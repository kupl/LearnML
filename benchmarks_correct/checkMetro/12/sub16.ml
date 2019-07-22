(* 8 checkMetro: metro -> bool *)
type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
and name = string

let checkMetro metro =
	let rec cMetro metro areas = match metro with
		STATION n -> List.mem n areas
		| AREA (n, m) -> cMetro m (n::areas)
		| CONNECT (m0, m1) -> (cMetro m0 areas) && (cMetro m1 areas) in
	
	cMetro metro []
