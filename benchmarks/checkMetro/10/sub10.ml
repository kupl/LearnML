type name = string 

type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro

;;

let rec check (me, li) =
	match me with
		STATION n -> List.mem n li
		| AREA (n, m) -> check (m, (n :: li))
		| CONNECT (m1, m2) -> check (m1, li) && check (m2, li)

let checkMetro me = check (me, [])

;;	