type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro 
and name = string

let checkMetro (met : metro) : bool =
	let rec check_aux (l : name list) (m : metro) : bool =
		match m with
		| STATION n -> List.mem n l
		| AREA (n1, m1) ->
			if (List.mem n1 l) then check_aux l m1
			else check_aux (n1::l) m1
		| CONNECT (m1, m2) -> (check_aux l m1) && (check_aux l m2)
	in
	check_aux [] met