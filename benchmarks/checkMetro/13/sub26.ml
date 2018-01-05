type metro =
	| STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec check_with_list m l =
	match m with
	| STATION str -> (List.mem str l)
	| CONNECT (a, b) -> (check_with_list a l) && (check_with_list b l)
	| AREA (str, met) -> check_with_list met (List.append l [str])

let rec checkMetro m = check_with_list m []