type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let rec checkMetro m = 
	let rec checkMetro2 m l =
		match m with
			(AREA (n, m2)) -> if (List.mem n l) then (checkMetro2 m2 l)
					  else (checkMetro2 m2 (n::l))
			|(STATION n) -> if (List.mem n l) then true
				       else false
			|(CONNECT (m1, m2)) -> (if (checkMetro2 m1 l) then (checkMetro2 m2 l)
					       else false) in
	checkMetro2 m []
	

