type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string;;

let checkMetro m =
	let rec ismem m lst = 
		match m with
		| STATION n -> List.mem n lst
		| AREA (n, m1) -> ismem m1 (n::lst)
		| CONNECT (m1, m2) -> ismem m1 lst && ismem m2 lst
	in
	ismem m [];;
