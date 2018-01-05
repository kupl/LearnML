type metro = 	STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro

and name = string


let rec checkMetro (m: metro) : bool =
	let rec checkArea((mc: metro), (l: string list)) : bool =
		match mc with
		| STATION nd -> List.mem nd l
		| CONNECT (md1, md2) -> checkArea(md1, l) && checkArea(md2, l)
		| AREA (nd, md) -> checkArea(md, (List.append l [nd]))
		
	in
	match m with
	| STATION n -> false
	| CONNECT (m1, m2) -> checkMetro(m1) && checkMetro(m2)
	| AREA (ns, ms) -> checkArea(m, [])
