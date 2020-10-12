type lambda = V of var
		   | P of var * lambda
   		   | C of lambda * lambda
and var = string


let rec check lambda = 
	let rec checkiter (lambda, arealist) =
		match (lambda, arealist) with
			| (V var, arealist) -> List.mem var arealist
			| (P (areavar, next), arealist) -> checkiter (next, areavar::arealist)
			| (C (lambda1, lambda2), arealist) -> (match (checkiter (lambda1, arealist), checkiter (lambda2, arealist)) with
												| (true,true) -> true
												| _ -> false
												)

	in checkiter (lambda, [])
