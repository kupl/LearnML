type name = string
type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro

let checkMetro m =
	let rec checkMetroA (met, l) =  match met with
		STATION n -> List.exists (fun x -> x = n) l
	  | AREA(n, m) -> checkMetroA(m, n::l)
	  | CONNECT(m1, m2) ->
			if checkMetroA(m1, l) then
				if checkMetroA(m2, l) then true
				else false
			else false
	in
	
	checkMetroA(m, [])