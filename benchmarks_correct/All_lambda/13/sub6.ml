(* KIHWAN KANG HW01-4 *)

(* PREDEFINED TYPES *)
type lambda = 
	V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string
(* END OF PREDEFINED TYPES *)

let rec check one = 
	let rec checkStationWithAreaList (station, area_list) = 
		match (station, area_list) with
		|(_, []) -> false
		|(station, area_var::area_rest) -> 
			if station = area_var
			then true
			else checkStationWithAreaList (station, area_rest)
in
	let rec checkWithAreaList (one, area_list) = 
		match one with
		|V station -> checkStationWithAreaList (station, area_list)
		|P (area_var, area_lambda) -> checkWithAreaList (area_lambda, area_var::area_list)
		|C (front, rear) -> 
			checkWithAreaList (front, area_list)
			&& checkWithAreaList (rear, area_list)
in
	checkWithAreaList (one, [])
