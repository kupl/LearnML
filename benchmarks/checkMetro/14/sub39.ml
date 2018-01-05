exception TODO (*done done*)

type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec listSearch ((l, key): (string list * string)): bool= 
	match l with
	| [] -> false
	| hd::tl -> if hd = key then true else listSearch (tl, key)

let rec checkNameStation ((n, m): (string list * metro)): bool= 
	match m with
	| STATION sname -> listSearch (n, sname)
	| AREA (sname, subm) -> checkNameStation(sname::n, subm)
	| CONNECT (subm1, subm2) -> checkNameStation (n, subm1) && checkNameStation(n, subm2)

let rec checkMetro (m: metro): bool = 
	match m with
	| STATION n -> false
	| AREA (n, STATION sname) -> if n = sname then true else false
	| AREA (n, subm) -> checkNameStation ([n], subm)
	| CONNECT (subm1, subm2) -> checkMetro (subm1) && checkMetro (subm2)