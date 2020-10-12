type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string
;;

let rec check input =
	let checkStation (areaList, station_var) =
		if List.exists (fun x -> x = station_var) areaList then true
		else false
	in
	let rec checkArea (areaList, m) =
		match m with
		V value -> checkStation (areaList, value)
		| P (value, m_prime) -> checkArea (areaList @ [value], m_prime)
		| C (first, second) -> checkArea (areaList, first) && checkArea (areaList, second)
	in	
        match input with
        | V value -> checkStation ([], value)
	| P (value, second) -> checkArea ([value], second)
	| C (first,second) -> check first && check second
;;
