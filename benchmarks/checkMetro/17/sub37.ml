type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

let rec checkMetro (x : metro) : bool = 
	let rec checkMetroIter ((x : metro), (nlist : string list)) : bool = 
		match x with
		| STATION a -> (List.mem a nlist)
		| AREA (n, m) -> checkMetroIter (m, n :: nlist)
		| CONNECT (a, b) -> (checkMetroIter (a, nlist)) && (checkMetroIter (b, nlist))
	in
	checkMetroIter(x, [])
