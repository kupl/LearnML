type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro m =
	let rec cmhelper m l =
		match m with
			| STATION(a) -> List.exists (function x -> a = x) l
			| AREA(a,b) -> cmhelper b (a :: l)
			| CONNECT(a,b) -> (cmhelper a l) && (cmhelper b l)
	in
	cmhelper m []