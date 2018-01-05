type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string


let rec comparing (name, li) =
	match li with
	| [] -> false
	| st::li' ->
		if(name = st) then true else comparing(name, li')
		
	let rec buff (under, li) =
		match under with
		| STATION st_name -> comparing(st_name, li)
		| AREA(id, m) -> buff ( m, List.append li [id])
		| CONNECT(m1, m2) -> (buff (m1, li) ) && (buff(m2, li))
		

let rec checkMetro under = buff(under, [])
		