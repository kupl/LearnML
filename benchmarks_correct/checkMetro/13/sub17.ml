type metro = STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro
and name = string

let checkMetro a = 
	let rec contains x lst = 
		match lst with
		| [] -> false
		| hd::rest -> if hd=x then true
				else contains x rest
	in
	let rec realcheckMetro a lst =
		match a with
		| STATION x -> contains x lst
		| AREA (x, y) -> realcheckMetro y (x::lst)
		| CONNECT (x, y) -> (realcheckMetro x lst) && (realcheckMetro y lst)
	in
	realcheckMetro a []

	
