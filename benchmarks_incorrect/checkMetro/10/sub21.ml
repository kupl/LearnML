type metro = STATION of name
	     | AREA of name * metro
	     | CONNECT of metro * metro
 and name = string

let rec checkMetro met =
	let rec check met lst = 
	match met with 
	STATION(id) -> List.mem id lst 
	| AREA(id, m) -> check m (lst @ [id])
	| CONNECT(m1, m2) -> check m1 lst & check m2 lst
	in
	match met with 
	STATION(id) -> false
	| AREA(id, m) -> check m [id] 
	| CONNECT(m1, m2) -> false
	;;


