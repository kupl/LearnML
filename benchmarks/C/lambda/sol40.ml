exception Error of string
type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let check m =
	let rec check_in m areas =
		match m with
			V st ->
				let rec is_valid st area_list = 
					(match area_list with
						[] -> false
						| h::t -> if (h = st) then true
								  else is_valid st t)
				in
					is_valid st areas
			| P (st, m') -> check_in m' (st::areas)
			| C (m1, m2) -> (check_in m1 areas) && (check_in m2 areas)
	in
		check_in m []
;;	
