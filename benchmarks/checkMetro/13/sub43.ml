type metro = STATION of name
					| AREA of name * metro
					| CONNECT of metro * metro
	and name = string

let checkMetro = fun mm ->
	let rec checkMetroL = fun mmm l ->
		match mmm with
			STATION(name) -> (List.mem name l)
			| AREA(name,m) -> (checkMetroL m (l@[name]))
			| CONNECT(m1, m2) -> ((checkMetroL m1 l) && (checkMetroL m2 l))
	in checkMetroL mm []
