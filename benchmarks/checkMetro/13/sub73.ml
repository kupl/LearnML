
type metro = STATION of name
						| AREA of name * metro
						| CONNECT of metro * metro
and name = string

let checkMetro : metro -> bool = fun metro -> 
	let rec checkMetroInner metro metroList = 
		match metro with
		| STATION(name) ->	
			List.mem name metroList
		| AREA(name, m) -> 
			checkMetroInner m (name::metroList)
		| CONNECT(m1, m2) -> 
			(checkMetroInner m1 metroList)&&(checkMetroInner m2 metroList)
	in 
	
	checkMetroInner metro []
