(* Ex 1 *)
type exp = X
	 | INT of int
	 | REAL of float
	 | ADD of exp * exp
	 | SUB of exp * exp
	 | MUL of exp * exp
	 | DIV of exp * exp
	 | SIGMA of exp * exp * exp
	 | INTEGRAL of exp * exp * exp

exception FREE_VARIABLE

let rec mm((e:exp), (eval:float)) = match e with
				  | X -> eval
				  | INT(i) -> float_of_int i 
				  | REAL(f) -> f
				  | ADD(exp1, exp2) -> mm(exp1, eval) +. mm(exp2, eval)
				  | SUB(exp1, exp2) -> mm(exp1, eval) -. mm(exp2, eval)
				  | MUL(exp1, exp2) -> mm(exp1, eval) *. mm(exp2, eval)
				  | DIV(exp1, exp2) -> mm(exp1, eval) /. mm(exp2, eval)
				  | SIGMA(e1, e2, e3) -> 
					( match e3 with
					| SIGMA(e, ee, eee) -> if int_of_float(mm(e1, eval)) > int_of_float(mm(e2, eval))
							       then 0.
							       else mm(SIGMA(ADD(INT(1), e1), e2, e3), eval)
							         +. mm(SIGMA(INT(int_of_float(mm(e, mm(e1, eval)))), INT(int_of_float(mm(ee, mm(e1, eval)))), eee), float_of_int(int_of_float(mm(e1, eval))))
					| INTEGRAL(e, ee, eee) -> if int_of_float(mm(e1, eval)) > int_of_float(mm(e2, eval))
								  then 0.
								  else mm(SIGMA(ADD(INT(1), e1), e2, e3), eval)
								    +. mm(INTEGRAL(REAL(mm(e, mm(e1, eval))), REAL(mm(ee, mm(e1, eval))), eee), float_of_int(int_of_float(mm(e1, eval))))
					| _ -> if int_of_float(mm(e1, eval)) > int_of_float(mm(e2, eval))
					       then 0.
					       else mm(SIGMA(ADD(INT(1), e1), e2, e3), eval)
					         +. mm(e3, float_of_int(int_of_float(mm(e1, eval))))
					)
				  | INTEGRAL(e1, e2, e3) -> 
					( match e3 with
					| SIGMA(e, ee, eee) -> if (mm(e2, eval) -. mm(e1, eval)) > -0.1 && (mm(e2, eval) -. mm(e1, eval) < 0.1)
							       then 0.
							       else if mm(e1, eval) > mm(e2, eval)
							       then 0. -. mm(INTEGRAL(e2, e1, e3), eval)
							       else mm(INTEGRAL(ADD(REAL(0.1), e1), e2, e3), eval) 
							         +. mm(MUL(REAL(0.1), REAL(mm(SIGMA(INT(int_of_float(mm(e, mm(e1, eval)))), INT(int_of_float(mm(ee, mm(e1, eval)))), eee), mm(e1, eval)))), eval)
					| INTEGRAL(e, ee, eee) -> if (mm(e2, eval) -. mm(e1, eval)) > -0.1 && (mm(e2, eval) -. mm(e1, eval) < 0.1)
								  then 0.
								  else if mm(e1, eval) > mm(e2, eval)
								  then 0. -. mm(INTEGRAL(e2, e1, e3), eval)
								  else mm(INTEGRAL(ADD(REAL(0.1), e1), e2, e3), eval) 
								    +. mm(MUL(REAL(0.1), REAL(mm(INTEGRAL(REAL(mm(e, mm(e1 ,eval))), REAL(mm(ee, mm(e1, eval))), eee), mm(e1, eval)))), eval)
					| _ -> if (mm(e2, eval) -. mm(e1, eval)) > -0.1 && (mm(e2, eval) -. mm(e1, eval) < 0.1)
					       then 0.
					       else if mm(e1, eval) > mm(e2, eval)
					       then 0. -. mm(INTEGRAL(e2, e1, e3), eval)
					       else mm(INTEGRAL(ADD(REAL(0.1), e1), e2, e3), eval) 
					         +. mm(MUL(REAL(0.1), REAL(mm(e3, mm(e1, eval)))), eval)
					)
let rec mathemadiga exp = match exp with
			| X -> raise FREE_VARIABLE
			| INT(i) -> float_of_int i
			| REAL(f) -> f
			| ADD(exp1, exp2) -> mathemadiga(exp1) +. mathemadiga(exp2)
			| SUB(exp1, exp2) -> mathemadiga(exp1) -. mathemadiga(exp2)
			| MUL(exp1, exp2) -> mathemadiga(exp1) *. mathemadiga(exp2)
			| DIV(exp1, exp2) -> mathemadiga(exp1) /. mathemadiga(exp2)
			| SIGMA(e1, e2, e3) -> 
				( match e3 with
				| SIGMA(e, ee, eee) -> if int_of_float(mathemadiga(e1)) > int_of_float(mathemadiga(e2))
						       then 0.
						       else mathemadiga(SIGMA(ADD(INT(1), e1), e2, e3)) 
							 +. mm(SIGMA(INT(int_of_float(mm(e, float_of_int(int_of_float(mathemadiga(e1)))))), INT(int_of_float(mm(ee, float_of_int(int_of_float(mathemadiga(e1)))))), eee), float_of_int(int_of_float(mathemadiga(e1))))
				| INTEGRAL(e, ee, eee) -> if int_of_float(mathemadiga(e1)) > int_of_float(mathemadiga(e2))
						          then 0.
							  else mathemadiga(SIGMA(ADD(INT(1), e1), e2, e3))
							    +. mm(INTEGRAL(REAL(mm(e, float_of_int(int_of_float(mathemadiga(e1))))), REAL(mm(ee, float_of_int(int_of_float(mathemadiga(e1))))), eee), float_of_int(int_of_float(mathemadiga(e1))))
				| _ -> if int_of_float(mathemadiga(e1)) > int_of_float(mathemadiga(e2))
				       then 0.
				       else mathemadiga(SIGMA(ADD(INT(1), e1), e2, e3)) 
					 +. mm(e3, float_of_int(int_of_float(mathemadiga(e1))))
				)
			| INTEGRAL(e1, e2, e3) -> 
				( match e3 with
				  | SIGMA(e, ee, eee) -> if (mathemadiga(e2) -. mathemadiga(e1) > -0.1) && (mathemadiga(e2) -. mathemadiga(e1) < 0.1)
				      		         then 0.
							 else if mathemadiga(e1) > mathemadiga(e2) 
							 then 0. -. mathemadiga(INTEGRAL(e2, e1, e3))
							 else mathemadiga(INTEGRAL(ADD(REAL(0.1), e1), e2, e3))
							   +. mathemadiga(MUL(REAL(0.1), REAL(mm(SIGMA(INT(int_of_float(mm(e, mathemadiga(e1)))), INT(int_of_float(mm(ee, mathemadiga(e1)))), eee), mathemadiga(e1)))))
				  | INTEGRAL(e, ee, eee) -> if (mathemadiga(e2) -. mathemadiga(e1) > -0.1 && mathemadiga(e2) -. mathemadiga(e1) < 0.1)
							    then 0.
							    else if mathemadiga(e1) > mathemadiga(e2) 
							    then 0. -. mathemadiga(INTEGRAL(e2, e1, e3))
							    else mathemadiga(INTEGRAL(ADD(REAL(0.1), e1), e2, e3))
							      +. mathemadiga(MUL(REAL(0.1), REAL(mm(INTEGRAL(REAL(mm(e, mathemadiga(e1))), REAL(mm(ee, mathemadiga(e1))), eee), mathemadiga(e1)))))
			          | _ -> if (mathemadiga(e2) -. mathemadiga(e1) > -0.1 && mathemadiga(e2) -. mathemadiga(e1) < 0.1)
					 then 0.
					 else if mathemadiga(e1) > mathemadiga(e2) 
					 then 0. -. mathemadiga(INTEGRAL(e2, e1, e3))
					 else mathemadiga(INTEGRAL(ADD(REAL(0.1), e1), e2, e3))
					   +. mathemadiga(MUL(REAL(0.1), REAL(mm(e3, mathemadiga(e1)))))
				)