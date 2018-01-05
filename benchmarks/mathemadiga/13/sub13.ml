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

let rec gassign (exp, value) = 
	match exp with X -> value
	| INT a -> exp
	| REAL a -> exp
	| ADD (a,b) -> ADD(gassign (a, value),gassign (b, value))
	| SUB (a,b) -> SUB(gassign (a, value),gassign (b, value))
	| MUL (a,b) -> MUL(gassign (a, value),gassign (b, value))
	| DIV (a,b) -> DIV(gassign (a, value),gassign (b, value))
	| SIGMA (a,b,f) -> SIGMA (gassign (a, value),gassign (b, value),f)
	| INTEGRAL (a,b,f) -> INTEGRAL (gassign (a, value),gassign (b, value),f)
	
let rec galculator (exp) =
	match exp with X -> raise FreeVariable
	| INT a -> float_of_int a
	| REAL a -> a
	| ADD (a,b) -> (galculator a) +. (galculator b)
	| SUB (a,b) -> (galculator a) -. (galculator b)
	| MUL (a,b) -> (galculator a) *. (galculator b)
	| DIV (a,b) -> (galculator a) /. (galculator b)
	| SIGMA (a,b,f) -> 
		if int_of_float(galculator a)>int_of_float(galculator b) then 0.
		else (galculator (gassign (f,  (INT (int_of_float(galculator a))))))
				+. (galculator (SIGMA (INT (int_of_float(galculator a)+1), b, f)))
	| INTEGRAL (a,b,f) ->
		if ((galculator b) < (galculator a)) then galculator (INTEGRAL (b,a,MUL(f,REAL(-1.0))))
		else if ((galculator b) -. (galculator a)) < 0.1 then 0.
		else (galculator (gassign (f,  REAL (galculator a))) *. 0.1)
				+. (galculator (INTEGRAL (REAL ((galculator a) +. 0.1), b, f)))
