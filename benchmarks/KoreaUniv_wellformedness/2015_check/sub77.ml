	type exp = V of var
			 | P of var * exp
			 | C of exp * exp
	and var = string

	let rec elist : exp -> var list
	= fun exp ->
		match exp with
		 | V var -> []
		 | P (var, exp1) -> 
		  (match exp1 with
		   | V var2 -> [var]
		   | P (var2, exp2) -> [var;var2]@(elist exp2)
		   | C (exp3, exp4) -> [var]@(elist exp3)@(elist exp4)
		  )
		 | C (exp2, exp3) -> 
		  (match exp2 with
		   | V var -> elist exp3
		   | P (var2, exp4) -> [var2]@(elist exp4)
		   | C (exp5, exp6) -> (elist exp5)@(elist exp6)@(elist exp3)
		  )

	let rec vlist exp =
		match exp with
		 | V var -> [var]
		 | P (var, exp1) -> 
		  (match exp1 with
		   | V var2 -> [var2]
		   | P (var2, exp2) -> vlist exp2
		   | C (exp3, exp4) -> (vlist exp3)@(vlist exp4)
		  )
		 | C (exp2, exp3) -> 
		  (match exp2 with
		   | V var -> [var]@(vlist exp3)
		   | P (var2, exp4) -> vlist exp4
		   | C (exp5, exp6) -> (vlist exp5)@(vlist exp6)@(vlist exp3)
		  )
		  
		  
	let rec exist explist varlist =
		match varlist with
		 | [] -> true
		 | vhd::vtl -> 
		  (match explist with
		   | [] ->  false
		   | ehd::etl ->  if vhd = ehd then exist explist vtl 
						  else exist etl [vhd] || exist etl varlist 
		  )
		  
	let rec check : exp -> bool
	=fun exp ->
	 exist (elist exp) (vlist exp)
