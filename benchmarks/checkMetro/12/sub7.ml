type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro m =
	let rec _check(m,al) = match m with
		STATION(name) -> List.mem name al
		| AREA(name,m') -> _check(m',name::al)
		| CONNECT(m1,m2) -> _check(m1,al) && _check(m2,al)
	in _check(m,[])
