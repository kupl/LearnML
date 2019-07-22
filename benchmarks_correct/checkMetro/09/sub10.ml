type metro = STATION of name
			|AREA of name * metro
			|CONNECT of metro * metro
and name = string

let rec checkMetro((m:metro)) =
	let rec check ((ma:metro), (li:name list))=
		match ma with
		|AREA(st, k) -> check(k, st::li)
		|CONNECT(me1, me2) -> check(me1, li) && check(me2, li)
		|STATION(na) -> List.mem na li
	in
	check(m, [])
