(* C:\Users\saigoy\Desktop\checkMetro.ml *)

type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
  and name = string;;

let checkMetro : metro -> bool = fun metro ->
  	let rec isProperStation (name, areaList) = 
  	match areaList with
  	| [] -> false
  	| hd::tl -> 
  	(
  		if(hd= name) then true
  		else (isProperStation (name, tl))
  	)	in
  
  	let rec checkMetro_Aux (metro, areaList) = 
  	match metro with
  	| STATION n -> isProperStation(n, areaList)
  	| AREA (n, m) -> checkMetro_Aux (m, n::areaList)
  	| CONNECT (lm, rm) -> ( checkMetro_Aux(lm, areaList) )&& ( checkMetro_Aux(rm, areaList) )	in
  checkMetro_Aux(metro, []);;

