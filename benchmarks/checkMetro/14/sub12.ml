type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string
;;

let rec checkMetro input =
	let checkStation (areaList, station_name) =
		if List.exists (fun x -> x = station_name) areaList then true
		else false
	in
	let rec checkArea (areaList, m) =
		match m with
		STATION value -> checkStation (areaList, value)
		| AREA (value, m_prime) -> checkArea (areaList @ [value], m_prime)
		| CONNECT (first, second) -> checkArea (areaList, first) && checkArea (areaList, second)
	in	
        match input with
        | STATION value -> checkStation ([], value)
	| AREA (value, second) -> checkArea ([value], second)
	| CONNECT (first,second) -> checkMetro first && checkMetro second
;;
