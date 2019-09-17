type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let rec check m = 
	let rec check2 m l =
		match m with
			(P (n, m2)) -> if (List.mem n l) then (check2 m2 l)
					  else (check2 m2 (n::l))
			|(V n) -> if (List.mem n l) then true
				       else false
			|(C (m1, m2)) -> (if (check2 m1 l) then (check2 m2 l)
					       else false) in
	check2 m []
	

