type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let checkMetro = fun metro ->
	let rec checkhelper = fun met nlist ->
		match met with
		|AREA(id, m) -> (checkhelper m (id::nlist))
		|STATION n ->
			(if (List.mem n nlist) then true
			else false)
		|CONNECT(m1, m2) -> (checkhelper m1 nlist) && (checkhelper m2 nlist)
	in (checkhelper metro [])
