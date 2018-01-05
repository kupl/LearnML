type metro = STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro
and name = string


let isIn (str, areas) =
	if (List.filter (function a -> (String.compare a str) == 0) areas) == [] then false else true

let rec subChkMet (met, areas) =
	match met with
	| STATION n -> if (isIn (n, areas)) then true else false
	| AREA (n, smet) -> subChkMet(smet, n::areas)
	| CONNECT (met1, met2) -> if ((subChkMet (met1, areas))&&(subChkMet (met2, areas))) then true else false

let checkMetro met =
        subChkMet (met, [])
