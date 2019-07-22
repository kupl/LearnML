type metro =
	STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
	and name = string
let rec check metro namelist =
	match metro with
	| STATION name -> List.exists (fun x -> x = name) namelist
	| AREA (a, b) -> check b (a::namelist)
	| CONNECT (a, b) -> (check a namelist) && (check b namelist)
let checkMetro metro =
	check metro []