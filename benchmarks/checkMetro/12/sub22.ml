type metro =
	  STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro

and name = string

let checkMetro met =
	let rec checkWithList (m, l) = match m with
		| STATION n -> List.mem n l
		| AREA (n, m') -> checkWithList (m', n::l)
		| CONNECT (m1, m2) -> checkWithList (m1, l) && checkWithList (m2, l)
	in
	checkWithList (met, [])