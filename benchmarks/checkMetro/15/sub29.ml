type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string

let rec checkSt_in_Metro (name,metro) =
	match metro with
	|STATION nm -> if(nm = name) then true
				   else false
	|AREA (nm,met) -> checkSt_in_Metro(name,met)
	|CONNECT (met1,met2) -> checkSt_in_Metro(name,met1)||checkSt_in_Metro(name,met2)

let rec makeMetro_wo_St (name,metro) =
	match metro with
	|STATION nm -> if(nm = name) then STATION "2011-13343ysh"
				   else STATION nm
	|AREA (nm,met) -> AREA(nm,makeMetro_wo_St(name,met))
	|CONNECT (met1,met2) -> CONNECT(makeMetro_wo_St(name,met1),makeMetro_wo_St(name,met2))

let rec checkMetro m =
	match m with
	|STATION name -> if(name = "2011-13343ysh") then true
					 else false
	|AREA (name,metro) -> 
		if(checkSt_in_Metro(name,metro)) then checkMetro (makeMetro_wo_St (name,metro))
		else checkMetro(metro)
	|CONNECT (metro1, metro2) -> checkMetro(metro1)&&checkMetro(metro2)
