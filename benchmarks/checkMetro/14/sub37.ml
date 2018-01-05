type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec checkMetro (m : metro) : bool =
	let area_list = [] in
	let rec check (mtr : metro) (al : name list) : bool =
		match mtr with
		| STATION n -> (List.mem n al)
		| AREA (n, mtr1) -> (check mtr1 (n::al))
		| CONNECT (mtr1, mtr2) -> (check mtr1 al) && (check mtr2 al) in
	(check m area_list)

