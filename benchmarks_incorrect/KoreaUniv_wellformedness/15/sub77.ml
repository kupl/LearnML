	type lambda = V of var
			 | P of var * lambda
			 | C of lambda * lambda
	and var = string

	let rec elist : lambda -> var list
	= fun lambda ->
		match lambda with
		 | V var -> []
		 | P (var, lambda1) -> 
		  (match lambda1 with
		   | V var2 -> [var]
		   | P (var2, lambda2) -> [var;var2]@(elist lambda2)
		   | C (lambda3, lambda4) -> [var]@(elist lambda3)@(elist lambda4)
		  )
		 | C (lambda2, lambda3) -> 
		  (match lambda2 with
		   | V var -> elist lambda3
		   | P (var2, lambda4) -> [var2]@(elist lambda4)
		   | C (lambda5, lambda6) -> (elist lambda5)@(elist lambda6)@(elist lambda3)
		  )

	let rec vlist lambda =
		match lambda with
		 | V var -> [var]
		 | P (var, lambda1) -> 
		  (match lambda1 with
		   | V var2 -> [var2]
		   | P (var2, lambda2) -> vlist lambda2
		   | C (lambda3, lambda4) -> (vlist lambda3)@(vlist lambda4)
		  )
		 | C (lambda2, lambda3) -> 
		  (match lambda2 with
		   | V var -> [var]@(vlist lambda3)
		   | P (var2, lambda4) -> vlist lambda4
		   | C (lambda5, lambda6) -> (vlist lambda5)@(vlist lambda6)@(vlist lambda3)
		  )
		  
		  
	let rec exist lambdalist varlist =
		match varlist with
		 | [] -> true
		 | vhd::vtl -> 
		  (match lambdalist with
		   | [] ->  false
		   | ehd::etl ->  if vhd = ehd then exist lambdalist vtl 
						  else exist etl [vhd] || exist etl varlist 
		  )
		  
	let rec check : lambda -> bool
	=fun lambda ->
	 exist (elist lambda) (vlist lambda)
