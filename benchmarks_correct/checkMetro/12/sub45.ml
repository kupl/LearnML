type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
	and name = string

let checkMetro mat =
	let rec chk mat stl =
		match mat with
		| STATION a -> (List.mem a stl)
		| AREA (a, b) -> (chk b (a::stl))
		| CONNECT (a, b) -> (chk a stl) && (chk b stl)
	in
	chk mat []


