type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string
let checkMetro: metro -> bool =
	let rec cM (met,lst) =
		match met with STATION n -> (List.mem n lst)
			|AREA (n,m) -> cM (m, n::lst)
			|CONNECT (m1,m2) -> (cM (m1,lst)) && (cM (m2,lst)) in
	fun m -> cM (m,[])
