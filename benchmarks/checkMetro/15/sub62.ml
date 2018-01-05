type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string;;

let checkMetro input =
	let rec aux arealist = function
	| STATION(m) -> (List.mem m arealist)
	| AREA(n, m) -> aux (n::arealist) m
	| CONNECT(m1, m2) -> (aux arealist m1) && (aux arealist m2) in
	aux [] input;;
