type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
  and name = string

let checkMetro : metro -> bool = fun m ->
	let rec checkmetro' = fun m namelist ->
		match m with
		| STATION name -> if (List.mem name namelist) then true
						  else false
		| AREA (name, metro) ->	checkmetro' metro (name::namelist)
		| CONNECT (met1, met2) -> checkmetro' met1 namelist && checkmetro' met2 namelist
	in
	checkmetro' m []


