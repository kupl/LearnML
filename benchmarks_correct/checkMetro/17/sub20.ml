type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro : metro -> bool = fun met ->

	let rec metroList stList mtr =
		match mtr with
		|STATION name -> List.mem name stList
		|AREA (name,metro) -> metroList (name::stList) metro
		|CONNECT (met1,met2)-> (metroList stList met1) && (metroList stList met2)

	in metroList [] met


