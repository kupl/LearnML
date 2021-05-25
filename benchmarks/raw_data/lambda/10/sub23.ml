type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string 

let check m = 
	 let rec check_sub m1 li = 
			match m1 with
			V a -> List.mem a li
			| P (var, met) -> (check_sub met (List.append li [var]))
			| C (met1, met2) -> (check_sub met1 li) && (check_sub met2 li) in
check_sub m [] 

	
