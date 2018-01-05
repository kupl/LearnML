(*2009-11718 1-7*)

type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec checkStation (a, lst) =
	match lst with
	[] -> []
	| hd::tl -> (if a=hd then checkStation(a, tl)
		else hd::checkStation(a, tl))

	
let rec checkMetro met =
	let rec makeMetro met lst =
		match met with
		STATION a -> a::lst
		| AREA (a, mtro) -> checkStation(a, (makeMetro mtro lst))
		| CONNECT (met1, met2) -> (makeMetro met1 lst)@(makeMetro met2 lst) in
	if (makeMetro met [])=[] then
	true
	else false

