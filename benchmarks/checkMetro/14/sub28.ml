type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec check_metro_rec m area_list=
	match m with
	| STATION s -> List.mem s area_list
	| AREA (n,m2) -> check_metro_rec m2 (n::area_list)
	| CONNECT (a,b) -> (check_metro_rec a area_list)&&(check_metro_rec b
			area_list)

let check_metro m=check_metro_rec m []
