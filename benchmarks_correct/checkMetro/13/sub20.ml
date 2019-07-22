type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec checkMetro m =
	let rec is_connect m =
		match m with
		| STATION _ -> false
		| CONNECT (_, _) -> true
		| AREA (name, metro) -> is_connect metro in
	
	let rec get_left m =
		match m with
		| STATION _ -> m
		| CONNECT (l, _) -> l
		| AREA (name, metro) -> AREA (name, get_left metro) in
	
	let rec get_right m =
		match m with
		| STATION _ -> m
		| CONNECT (_, r) -> r
		| AREA (name, metro) -> AREA (name, get_right metro) in


	if is_connect m then (checkMetro (get_left m)) && (checkMetro (get_right m))
	else match m with
	| STATION _ -> false
	| CONNECT (l, r) -> (checkMetro l) && (checkMetro r) 
	| AREA (name, metro) ->
		match metro with
		| STATION s -> if name=s then true
					else false
		| CONNECT (left, right) -> (checkMetro (AREA(name, left))) && (checkMetro (AREA(name, right)))
		| AREA (name2, metro2) -> (checkMetro (AREA(name, metro2))) || (checkMetro (AREA(name2, metro2)))










