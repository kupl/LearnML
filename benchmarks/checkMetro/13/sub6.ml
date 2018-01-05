(* KIHWAN KANG HW01-4 *)

(* PREDEFINED TYPES *)
type metro = 
	STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string
(* END OF PREDEFINED TYPES *)

let rec checkMetro one = 
	let rec checkStationWithAreaList (station, area_list) = 
		match (station, area_list) with
		|(_, []) -> false
		|(station, area_name::area_rest) -> 
			if station = area_name
			then true
			else checkStationWithAreaList (station, area_rest)
in
	let rec checkMetroWithAreaList (one, area_list) = 
		match one with
		|STATION station -> checkStationWithAreaList (station, area_list)
		|AREA (area_name, area_metro) -> checkMetroWithAreaList (area_metro, area_name::area_list)
		|CONNECT (front, rear) -> 
			checkMetroWithAreaList (front, area_list)
			&& checkMetroWithAreaList (rear, area_list)
in
	checkMetroWithAreaList (one, [])
