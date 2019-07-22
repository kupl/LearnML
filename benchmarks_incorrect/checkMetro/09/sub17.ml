type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let rec checkMetro m = 
	let rec append m1 m2 = 
	match m1 with
		(h::t) -> if (List.mem h m2) then (append t m2)
			  else (append t (h::m2))
		|[] -> m2 in
	let rec checkMetro2 m l = 
	match m with 
		(STATION n) -> l
		|(AREA (n, m2)) -> if (List.mem n l) then (checkMetro2 m2 l)
				   else (checkMetro2 m2 (n::l))	
		|(CONNECT (m1, m2)) -> (append (checkMetro2 m1 l) (checkMetro2 m2 l)) in
	let rec checkMetro3 m l = 
	match m with
		(STATION n) -> if (List.mem n l) then true
				else false
		|(AREA (n, m2)) -> (checkMetro3 m2 l)
		|(CONNECT (m1, m2)) -> (checkMetro3 m1 l) & (checkMetro3 m2 l) in
	let l =	checkMetro2 m [] in
	checkMetro3 m l		

