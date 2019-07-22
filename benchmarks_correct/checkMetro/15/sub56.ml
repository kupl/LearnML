type metro = STATION of name
		   	| AREA of name * metro
		   	| CONNECT of metro * metro
and name = string

let rec myCheck m l = 
	match m with
	| STATION n -> (List.mem n l)
	| AREA (n, mm) -> if (List.mem n l) then (myCheck mm l) else (myCheck mm (n::l))
	| CONNECT (mm1, mm2) -> (myCheck mm1 l) && (myCheck mm2 l)

let rec checkMetro m =
	myCheck m []
