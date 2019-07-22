type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string


let rec checkMetro_list m a_list =
	match m with
	| STATION s -> List.mem s a_list
	| AREA (a , m1) -> checkMetro_list m1 (a::a_list)
	| CONNECT (m2 , m3) -> (checkMetro_list m2 a_list) && (checkMetro_list m3 a_list)


let checkMetro m = checkMetro_list m []

