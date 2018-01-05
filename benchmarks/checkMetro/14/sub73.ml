type metro = STATION of name
			|AREA of name * metro
			|CONNECT of metro * metro
and name = string 

let rec contain (l, str) =
	match l with
		| [] -> false
		| h::t -> (h = str) || contain (t, str)

let rec checkMetro2 (s, m) =
	match m with 
		| STATION x -> contain (s, x)
		| CONNECT (m1, m2) -> (checkMetro2 (s, m1)) && (checkMetro2 (s, m2))
		| AREA (a, b) -> checkMetro2 (a::s, b)

let checkMetro m = checkMetro2 ([], m)	