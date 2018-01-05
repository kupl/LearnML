type metro = STATION of name
			|AREA of name * metro
			|CONNECT of metro * metro
and name = string 

let rec inner_check: (name list*metro) -> bool =
	fun (l, met) ->
		match met with 
			| STATION name ->  (List.mem name l)
			| CONNECT (m1, m2) -> (inner_check (l, m1)) && (inner_check (l, m2))
			| AREA (name, m) -> inner_check (name::l, m)

let checkMetro m = inner_check ([], m)	