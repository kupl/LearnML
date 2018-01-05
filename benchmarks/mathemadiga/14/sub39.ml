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

let rec cal_helper (e, n) =
	match e with
	| X -> n
	| INT i -> INT i
	| REAL r -> REAL r
	| ADD(e1,e2) -> ADD(cal_helper (e1, n) , cal_helper (e2, n))
	| SUB(e1,e2) -> SUB(cal_helper (e1, n) , cal_helper (e2, n))
	| MUL(e1,e2) -> MUL(cal_helper (e1, n) , cal_helper (e2, n))
	| DIV(e1,e2) -> DIV(cal_helper (e1, n) , cal_helper (e2, n))
	| SIGMA(e1,e2,e3) -> SIGMA(cal_helper (e1, n) , cal_helper (e2, n) , e3)
	| INTEGRAL(e1,e2,e3) -> INTEGRAL(cal_helper (e1, n) , cal_helper (e2, n), e3)

let rec galculator e =
	match e with
	| X -> raise FreeVariable
	| INT i -> float_of_int(i)
	| REAL r -> r
	| ADD(e1,e2) -> galculator(e1) +. galculator(e2)
	| SUB(e1,e2) -> galculator(e1) -. galculator(e2)
	| MUL(e1,e2) -> galculator(e1) *. galculator(e2)
	| DIV(e1,e2) -> galculator(e1) /. galculator(e2)
	| SIGMA(e1,e2,e3) -> let n = int_of_float(galculator e1) in let m = int_of_float(galculator e2) in
						 if n > m then 0.0
						 else (galculator (cal_helper (e3 , INT n)))
								+. (galculator (SIGMA ((INT (n + 1) , INT m , e3))))
	| INTEGRAL(e1,e2,e3) -> let a = galculator e1 in let b = galculator e2 in
							if a > b then galculator (INTEGRAL (REAL b , REAL a , SUB(REAL 0.0 , e3)))
							else if a +. 0.1 > b then 0.0
							else (galculator (cal_helper (e3, REAL a)) *. 0.1)
									+. (galculator (INTEGRAL ((REAL (a +. 0.1), REAL b , e3))))


	
