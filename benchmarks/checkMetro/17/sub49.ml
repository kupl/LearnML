type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec withlist (m: metro) (nl: string list): bool = 
	match m with
	| AREA(n, me) -> (withlist me (n::nl))
	| CONNECT(m1, m2) -> (withlist m1 nl) && (withlist m2 nl)
	| STATION(n) -> (List.mem n nl)

let rec checkMetro (m: metro): bool =
	(withlist m []) 