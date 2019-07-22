type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string


let rec check id = function
	| [] -> false
	| hd::tl ->
	  if hd=id then true
	  else check(id)(tl)

let rec foo areas = function
	| STATION (id) -> check(id)(areas)
	| AREA (id, m) -> foo([id]@areas)(m)
	| CONNECT (m1, m2) -> foo(areas)(m1) && foo(areas)(m2)

let rec checkMetro = foo []



