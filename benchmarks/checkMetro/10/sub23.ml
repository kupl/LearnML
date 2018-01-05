type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string 

let checkMetro m = 
	 let rec checkMetro_sub m1 li = 
			match m1 with
			STATION a -> List.mem a li
			| AREA (name, met) -> (checkMetro_sub met (List.append li [name]))
			| CONNECT (met1, met2) -> (checkMetro_sub met1 li) && (checkMetro_sub met2 li) in
checkMetro_sub m [] 

	
