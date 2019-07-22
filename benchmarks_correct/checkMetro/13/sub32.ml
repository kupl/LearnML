type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro met = 
	let rec subcheckMetro m areas = 
		match m with 
			STATION(name) -> (List.mem name areas)
			| AREA(name, met1) -> (subcheckMetro met1 (name::areas))
			| CONNECT(met1, met2) -> (subcheckMetro met1 areas) && (subcheckMetro met2 areas)
	in (subcheckMetro met [])


