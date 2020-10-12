type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
	and var = string
let check map =
	let rec check' (arealist, map) = 
		match map with
		| V var -> if (List.mem var arealist) then true else false
		| C (map1, map2) -> check' (arealist, map1) && check' (arealist, map2)
		| P (var, map') -> check' (var::arealist, map')
	in
	check' ([], map)
