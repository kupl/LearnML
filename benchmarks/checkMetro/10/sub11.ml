type metro = STATION of name
			|AREA of name * metro
			|CONNECT of metro * metro
	and name = string

let rec checkMetro met =
	checkMetro2 met []
	and checkMetro2 met strs = 
	match met with
	STATION n -> if List.mem n strs	then true else false
	|AREA (n,m) -> checkMetro2 m (n::strs)
	|CONNECT (m1, m2) -> (checkMetro2 m1 strs) && (checkMetro2 m2 strs)
	
