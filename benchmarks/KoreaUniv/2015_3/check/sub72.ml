type exp = V of var
				| P of var * exp
				| C of exp * exp
     and var = string
		 let rec checktest a b = match a with 
																	 |V x -> List.mem x b
																	 |P (x, y) -> checktest y (x::b)
																	 |C (x, y) -> checktest x b && checktest y b;;
		 let check exp = checktest exp [];;
