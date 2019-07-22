type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
and name = string

let rec checkMetro metro = 
	let rec checkMetroSub metro name_list = match metro with
					      | AREA (name, m) -> checkMetroSub m (name_list @ [name])
					      | CONNECT (m1, m2) -> (checkMetroSub m1 name_list) && (checkMetroSub m2 name_list)
					      | STATION name -> List.mem name name_list
	in checkMetroSub metro []
