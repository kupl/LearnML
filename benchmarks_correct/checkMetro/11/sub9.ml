type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro : metro -> bool = fun startm ->
	let rec checkMetro' : metro * name list -> bool = fun (m, l) ->
		match m with
		| STATION n ->
			List.mem n l
		| AREA (n1, m2) ->
			if (List.mem n1 l) = false then checkMetro'(m2, n1::l)
			else checkMetro'(m2, l)
		| CONNECT (m1, m2) ->
			checkMetro'(m1, l) && checkMetro'(m2, l)
	in
		let startl : name list = []
		in
			checkMetro' (startm, startl)