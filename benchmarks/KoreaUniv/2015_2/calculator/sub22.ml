type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator exp =
	match exp with
	 | X -> raise (Failure "nothing to calculate") 
	 | INT n -> n
	 | ADD (exp1,exp2) -> (calculator exp1) + (calculator exp2)
	 | SUB (exp1,exp2) -> (calculator exp1) - (calculator exp2)
	 | MUL (exp1,exp2) -> (calculator exp1) * (calculator exp2)
	 | DIV (exp1,exp2) -> if (calculator exp2) = 0 then raise (Failure "divided by zero") 
						  else (calculator exp1) / (calculator exp2)
	 | SIGMA (exp1,exp2,exp3) -> if (calculator exp1) > (calculator exp2) then raise (Failure "sigma range error") else
		(match exp3 with
		 | X -> if (calculator exp1) = (calculator exp2) then (calculator exp1)
				else (calculator (SIGMA (exp1,exp1,exp1))) + (calculator (SIGMA (INT (calculator (ADD (exp1,INT 1))),exp2,exp3)))
		 | INT n -> if (calculator exp1) = (calculator exp2) then (calculator exp3) 
					else  (calculator exp3) + (calculator (SIGMA (INT (calculator (ADD (exp1,INT 1))),exp2,exp3)))
		 | ADD (exp4,exp5) -> (calculator (SIGMA (exp1,exp2,exp4))) + (calculator (SIGMA (exp1,exp2,exp5)))
		 | SUB (exp4,exp5) -> (calculator (SIGMA (exp1,exp2,exp4))) - (calculator (SIGMA (exp1,exp2,exp5)))
		 | MUL (exp4,exp5) ->  if 	(calculator exp1) = (calculator exp2) then 
									(calculator (SIGMA (exp1,exp1,exp4))) * (calculator (SIGMA (exp1,exp1,exp5)))
							   else (calculator (SIGMA (exp1,exp1,exp4))) * (calculator (SIGMA (exp1,exp1,exp5))) +
									(calculator (SIGMA (INT (calculator (ADD (exp1,INT 1))),exp2,exp3)))
		 | DIV (exp4,exp5) -> if (calculator exp5) = 0 then raise (Failure "divided by zero")
							  else if 	(calculator exp1) = (calculator exp2) then
										(calculator (SIGMA (exp1,exp1,exp4))) / (calculator (SIGMA (exp1,exp1,exp5)))
							  else  	(calculator (SIGMA (exp1,exp1,exp4))) / (calculator (SIGMA (exp1,exp1,exp5))) +
										(calculator (SIGMA (INT (calculator (ADD (exp1,INT 1))),exp2,exp3))) 
		 
		 | SIGMA (exp4,exp5,exp6) -> if (calculator exp4) > (calculator exp5) then raise (Failure "sigma range error") else
								(calculator (SIGMA (exp4,exp5,INT (calculator exp3))))
		)
		