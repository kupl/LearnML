type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
and name = string

let areas : string list = []

let rec checkM :(metro*string list)-> bool = fun (metro,areas) ->
	match metro with
	|STATION n -> List.mem n areas
	|AREA (n,m) -> let areas = n::areas in checkM(m,areas)
	|CONNECT (m1,m2) -> checkM(m1,areas) && checkM(m2,areas)

let rec checkMetro : metro -> bool = fun metro ->
	checkM(metro,areas)
