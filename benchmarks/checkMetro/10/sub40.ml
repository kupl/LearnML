exception Error of string
type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let checkMetro m =
	let rec checkMetro_in m areas =
		match m with
			STATION st ->
				let rec is_valid st area_list = 
					(match area_list with
						[] -> false
						| h::t -> if (h = st) then true
								  else is_valid st t)
				in
					is_valid st areas
			| AREA (st, m') -> checkMetro_in m' (st::areas)
			| CONNECT (m1, m2) -> (checkMetro_in m1 areas) && (checkMetro_in m2 areas)
	in
		checkMetro_in m []
;;	
