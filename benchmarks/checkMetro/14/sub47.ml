type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec checkMetroProc (met:metro) (l:name list) : bool =
	match met with
	| STATION n -> (List.mem n l)
	| AREA (n,m) -> (checkMetroProc m (l@[n]))
	| CONNECT (m1,m2) -> (checkMetroProc m1 l)&&(checkMetroProc m2 l)

let checkMetro (met:metro) : bool =
	match met with
	| STATION n -> false
	| AREA (n,m) -> (checkMetroProc m [n])
	| CONNECT (m1,m2) -> (checkMetroProc m1 [])&&(checkMetroProc m2 [])
