type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string;; 

let rec checkMetroG lst met =
	match met with
	| AREA(n, m) -> checkMetroG (n::lst) m
	| CONNECT(m1, m2) -> checkMetroG lst m1 && checkMetroG lst m2
	| STATION t ->
		if List.mem t lst then true
		else false;;

let checkMetro = checkMetroG [];;	