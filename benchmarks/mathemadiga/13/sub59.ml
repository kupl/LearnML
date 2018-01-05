type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp*exp
		 | SUB of exp*exp
		 | MUL of exp*exp
		 | DIV of exp*exp
		 | SIGMA of exp*exp*exp
		 | INTEGRAL of exp*exp*exp

exception FreeVariable

let rec gal_x expr x =
  match expr with
    X -> if x=0.123 then raise FreeVariable else x
  | INT a -> (float_of_int a)
  | REAL a -> a
  | ADD(e1, e2) -> (gal_x e1 x)+.(gal_x e2 x)
  | SUB(e1, e2) -> (gal_x e1 x)-.(gal_x e2 x)
  | MUL(e1, e2) -> (gal_x e1 x)*.(gal_x e2 x)
  | DIV(e1, e2) -> (gal_x e1 x)/.(gal_x e2 x)
  | SIGMA(a_e, b_e, e) -> let a = (gal_x a_e x) in
  					  	  let b = (gal_x b_e x) in
  					  		if a<b then (gal_x (SIGMA((INT ((int_of_float a)+1)), (INT (int_of_float b)), e)) x)+.
										(gal_x (SIGMA((INT (int_of_float a)), (INT (int_of_float a)), e)) x)
  					  		else if a>b then 0.0
					  		else (gal_x e a)
  | INTEGRAL(a_e, b_e, e) -> let a = (gal_x a_e x) in
  						 	 let b = (gal_x b_e x) in
  						       if a+.0.1<=b then (gal_x (INTEGRAL((REAL (a+.0.1)), (REAL b), e)) x)+.(gal_x e a)*.0.1
							   else if b+.0.1<=a then (gal_x (INTEGRAL((REAL b), (REAL a), e)) x)*.(-1.0)
							   else 0.0

let galculator expr = (gal_x expr 0.123)
