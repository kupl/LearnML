type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro met =
	let rec chk (x, y) =
		match x with
		| STATION a -> List.mem a y
		| AREA (a, b) -> chk (b, a::y)
		| CONNECT (a, b) -> chk (a, y) && chk (b, y) in
	chk (met, [])

