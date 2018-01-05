type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string;;

let checkMetro met =
	let rec areaChecker (lst : string list) m b = 
		match m with
		| STATION n -> b && (List.mem n lst)
		| AREA (n, m1) -> b && (areaChecker (n::lst) m1 b)
		| CONNECT (m1, m2) -> b && (areaChecker lst m1 b) && (areaChecker lst m2 b) in
	areaChecker [] met true;;
