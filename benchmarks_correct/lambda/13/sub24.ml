type lambda = V of var
	   | P of var * lambda
	   | C of lambda * lambda
and var = string

let rec check lambda = 
	let rec checkSub lambda var_list = match lambda with
					      | P (var, m) -> checkSub m (var_list @ [var])
					      | C (m1, m2) -> (checkSub m1 var_list) && (checkSub m2 var_list)
					      | V var -> List.mem var var_list
	in checkSub lambda []
