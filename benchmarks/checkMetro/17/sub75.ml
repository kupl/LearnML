type metro = STATION of name
		|AREA of name * metro
		|CONNECT of metro * metro
and name = string

let rec checkMetroHelper ((m : metro), (l : string list)) : bool = 
    match m with 
    | STATION(s0) -> List.mem s0 l
    | AREA(s0, m0) -> checkMetroHelper(m0, (s0::l))
    | CONNECT(m1, m2) -> checkMetroHelper(m1, l) && checkMetroHelper(m2, l)

let checkMetro (m : metro) : bool =
	checkMetroHelper(m, [])
