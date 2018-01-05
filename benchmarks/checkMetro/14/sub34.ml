exception TODO

type metro = STATION of name

| AREA of name * metro
| CONNECT of metro * metro
and name = string
let rec checkMetro (m: metro) :bool =
	match m with
	| AREA (id, y) -> 
			if (checkMetro y) then true else 
				(match y with
				| STATION s -> s=id
				| AREA (a,metr) ->  
					(match metr with
					| CONNECT(m1,m2) -> (checkMetro (AREA(a,m1)) || checkMetro (AREA(id,m1)))
								&& (checkMetro (AREA(a,m2)) || checkMetro (AREA(id,m2)))
					|_ ->
						checkMetro (AREA(id,metr)) || checkMetro (AREA(a,metr)) 
					)
				| CONNECT (m1,m2) -> 
					checkMetro (AREA(id,m1)) && checkMetro (AREA(id,m2))

				)
	| CONNECT (m1,m2) -> (checkMetro m1) && (checkMetro m2)
	| _ -> false
