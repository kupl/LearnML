type metro =
	| STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec checkMetro (m: metro): bool = 
	let rec cm_iter (met: metro) (anl: name list): bool =
		match met with
		| STATION n -> List.mem n anl
		| AREA (p, q) -> cm_iter q (p::anl)
		| CONNECT (p, q) -> (cm_iter p anl) && (cm_iter q anl)
	in cm_iter m []