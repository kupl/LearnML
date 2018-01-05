type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec cMIter: metro * name list -> bool = fun (m, l) ->
	match m with
	STATION(n) -> List.mem n l
	| AREA(n, nm) -> cMIter (nm, n::l)
	| CONNECT(a, b) -> (cMIter (a, l)) && (cMIter (b, l))

let checkMetro: metro->bool = fun m ->
	cMIter (m, [])
