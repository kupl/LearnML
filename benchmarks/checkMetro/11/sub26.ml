type metro = STATION of name
	 | AREA of name * metro
	 | CONNECT of metro * metro
 and name = string

let rec check(metro, discovered) = 
	match metro with
 	STATION(n) -> List.mem n discovered
	| AREA(n,m) -> check(m, n::discovered) 
	| CONNECT(m1,m2) -> check(m1,discovered) && check(m2,discovered)

let checkMetro(metro) = 
	check(metro, [])

	
