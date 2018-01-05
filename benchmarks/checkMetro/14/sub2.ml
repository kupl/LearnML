(* 2006-11377 hw2-1 *)

type metro = 
STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro metro = 
	let rec cm (arlist, met) = 
		match met with
		| STATION name -> (List.mem name arlist)
		| AREA (aname, m) -> cm(aname::arlist, m)
		| CONNECT (m1, m2) -> cm(arlist, m1) && cm(arlist, m2)
	in	
	match metro with
	| STATION _ | CONNECT _ -> cm([], metro)
	| AREA (aname, m) -> cm([aname], m)
