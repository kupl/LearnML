type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp *exp
		 | INTEGRAL of exp * exp * exp
exception FreeVariable

let rec vartoVal (value, exp) =  (*value is exp*)
	match exp with
	| X -> value
	| INT i -> INT i
	| REAL r -> REAL r
	| ADD (e1,e2) -> ADD(vartoVal(value,e1),vartoVal(value,e2))
	| SUB (e1,e2) -> SUB(vartoVal(value,e1),vartoVal(value,e2))
	| MUL (e1,e2) -> MUL(vartoVal(value,e1),vartoVal(value,e2))
	| DIV (e1,e2) -> DIV(vartoVal(value,e1),vartoVal(value,e2))
	| SIGMA (e1,e2,e3) -> SIGMA(vartoVal(value,e1),vartoVal(value,e2),e3)
	| INTEGRAL (e1,e2,e3) -> INTEGRAL(vartoVal(value,e1),vartoVal(value,e2),e3)

let rec galculator exp = 
	match exp with
	| X -> raise(FreeVariable)
	| INT i -> float_of_int i
	| REAL r -> r
	| ADD (e1,e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1,e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1,e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1,e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1,e2,e3) -> if ((int_of_float (galculator e1)) > (int_of_float (galculator e2))) then 0.
						  else galculator (vartoVal (e1,e3)) +. galculator (SIGMA ((ADD((INT 1),e1)),e2,e3)) 
	| INTEGRAL (e1,e2,e3) -> if (abs_float((galculator e1) -. (galculator e2)) < 0.1) then 0.
						  	 else if ((galculator e1) > (galculator e2)) then -1. *. (galculator (INTEGRAL (e2,e1,e3)))
						     else 0.1 *. galculator (vartoVal (e1,e3)) +. galculator (INTEGRAL ((ADD((REAL 0.1),e1),e2,e3)))
