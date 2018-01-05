type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
			and name = string

let rec checkMetro a = 
	let rec checkm m ls =
		match m with
		STATION(name) -> List.mem name ls
		|AREA (n, m) -> checkm m (n::ls)
		|CONNECT(m1, m2) -> (checkm m1 ls) && (checkm m2 ls) in
	checkm a []
