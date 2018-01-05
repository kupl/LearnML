type metro 	= STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec validMetro (m , names) =
	match m with
	| STATION name -> List.exists (fun x -> x = name) names
	| AREA (name , m0) -> validMetro(m0,name::names)
	| CONNECT (m1,m2) -> validMetro(m1,names) && validMetro(m2,names)

let checkMetro m = 
	validMetro(m,[])
