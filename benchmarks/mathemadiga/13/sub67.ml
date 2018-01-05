exception FreeVariable
type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
let rec cformula(r,f) =
	match f with
	| X -> r
	| INT n -> float_of_int(n) 
	| REAL r -> r
	| ADD(e1,e2) -> cformula(r,e1)+.cformula(r,e2)
	| SUB(e1,e2) -> cformula(r,e1)-.cformula(r,e2)
	| MUL(e1,e2) -> cformula(r,e1)*.cformula(r,e2)
	| DIV(e1,e2) -> cformula(r,e1)/.cformula(r,e2)
	| SIGMA(e1,e2,f)-> if int_of_float(cformula(r,e1)) > int_of_float(cformula(r,e2)) then 0.0
	                   else cformula(float_of_int(int_of_float(cformula(r,e1))),f)+.cformula(r,SIGMA((INT(int_of_float(cformula(r,e1))+1)),(INT(int_of_float(cformula(r,e2)))),f))
	| INTEGRAL(e1,e2,f)-> if cformula(r,e2) < cformula(r,e1) then 0.0-.cformula(r,INTEGRAL(e2,e1,f))
						  else if cformula(r,e2)-.cformula(r,e1)<0.1 then 0.0
	                      else cformula(cformula(r,e1),f)*.0.1+.cformula(r,INTEGRAL((REAL(cformula(r,e1)+.0.1)),(REAL(cformula(r,e2))),f))
let rec galculator(e) =
	match e with
	| X -> raise FreeVariable
	| INT n -> float_of_int(n)
	| REAL r -> r
	| ADD(e1,e2) -> galculator(e1) +. galculator(e2)
	| SUB(e1,e2) -> galculator(e1) -. galculator(e2)
	| MUL(e1,e2) -> galculator(e1) *. galculator(e2)
	| DIV(e1,e2) -> galculator(e1) /. galculator(e2)
	| SIGMA(e1,e2,f)-> if int_of_float(galculator(e1)) > int_of_float(galculator(e2)) then 0.0
	                   else cformula(float_of_int(int_of_float(galculator(e1))),f)+.galculator(SIGMA((INT(int_of_float(galculator(e1))+1)),(INT(int_of_float(galculator(e2)))),f))
	| INTEGRAL(e1,e2,f)-> if galculator(e2) < galculator(e1) then 0.0-.galculator(INTEGRAL(e2,e1,f))
						  else if galculator(e2)-.galculator(e1)<0.1 then 0.0	
	                      else cformula(galculator(e1),f)*.0.1+.galculator(INTEGRAL((REAL(galculator(e1)+.0.1)),(REAL(galculator(e2))),f))
