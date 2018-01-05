(*2011-11004 ³²À±¼® ¹®Á¦ 1*)

type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec subCheck(s,a) =
	match s with
	STATION x -> 
		if List.mem x a then true
		else false
	| AREA (name, s2) -> subCheck(s2, name::a)
	| CONNECT (s2, s3) -> subCheck(s2, a) && subCheck(s3, a)
	

let rec checkMetro s =
	let alist = [] in
	subCheck(s, alist)
			

