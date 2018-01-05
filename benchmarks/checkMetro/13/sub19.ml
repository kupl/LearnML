type metro  = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
			and name = string
let rec checkMetro : metro -> bool = 
	let rec matchStr : name list * name -> bool =
		fun(namelist,name) ->
			match namelist with
			| h::t -> if(h=name) then true
					  else matchStr(t,name)
			| [] -> false
	in
	let rec checkDeeper : metro * name list -> bool = 
		fun(metro,namelist) ->
			match metro with
			|STATION name -> matchStr(namelist,name)
			|AREA (n,m) -> checkDeeper(m,[n]@namelist)
			|CONNECT (m1,m2)-> checkDeeper(m1,namelist)&&checkDeeper(m2,namelist)
	in
	fun metro -> checkDeeper(metro,[])
