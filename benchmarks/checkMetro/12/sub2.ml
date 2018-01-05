type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string;;

let checkMetro m =
	let rec ismem m list = 
		match m with
		| STATION n -> List.mem n list
		| AREA (n, m1) -> ismem m1 (n::list)
		| CONNECT (m1, m2) -> ismem m1 list && ismem m2 list
	in
	ismem m [];;