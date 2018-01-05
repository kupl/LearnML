type metro = STATION of name
        | AREA of name * metro
        | CONNECT of metro * metro
and name = string

let rec check (m, l) =
	match m with
	| AREA (nam, met) -> check (met, nam :: l)
	| STATION (nam) ->(
		try
			let x = (List.find (fun elem -> (elem = nam)) l) in true
		with  Not_found -> false
                )
	| CONNECT (met1, met2) -> check (met1, l) && check (met2, l)

let checkMetro m =
	check (m, [])
