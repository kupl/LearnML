exception FreeVariable

type exp = 
| X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec galculator_sub ((e: exp), (x: float)): float =
	match e with
	| X -> x
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> galculator_sub (e1, x) +. galculator_sub (e2, x)
	| SUB (e1, e2) -> galculator_sub (e1, x) -. galculator_sub (e2, x)
	| MUL (e1, e2) -> galculator_sub (e1, x) *. galculator_sub (e2, x)
	| DIV (e1, e2) -> galculator_sub (e1, x) /. galculator_sub (e2, x)
	| SIGMA (e1, e2, e3) -> let a = (int_of_float (galculator_sub (e1, x)))
	 						and b = (int_of_float (galculator_sub (e2, x))) in
	 						if (a>b) then 0.0
	 						else if (a==b) then galculator_sub (e3, (float_of_int a))
	 						else galculator_sub(e3, (float_of_int a)) +. galculator_sub((SIGMA (INT (a+1), INT b, e3)), (float_of_int (a+1)))
	| INTEGRAL (e1, e2, e3) -> let a = galculator_sub (e1, x)
							   and b = galculator_sub (e2, x) in
							   if (a-.b < 0.1 && a-.b > -0.1) then 0.0
							   else if (a>b) then (-.(galculator_sub (e3, b) +. galculator_sub((INTEGRAL((REAL (b+.0.1)), REAL a, e3)), b+.0.1))) 
							   else (0.1 *. galculator_sub(e3, a)) +. galculator_sub((INTEGRAL (REAL (a +.0.1), REAL b, e3)), a+.0.1)

let rec galculator (e: exp): float =
	match e with
	| X -> raise FreeVariable
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> galculator e1 +. galculator e2
	| SUB (e1, e2) -> galculator e1 -. galculator e2
	| MUL (e1, e2) -> galculator e1 *. galculator e2
	| DIV (e1, e2) -> galculator e1 /. galculator e2
	| SIGMA (e1, e2, e3) -> let a = (int_of_float (galculator e1))
	 						and b = (int_of_float (galculator e2)) in
	 						if (a>b) then 0.0
	 						else if (a==b) then galculator_sub (e3, (float_of_int a))
	 						else galculator_sub (e3, (float_of_int a)) +. galculator (SIGMA (INT (a+1), INT b, e3))
	| INTEGRAL (e1, e2, e3) -> let a = (galculator e1) 
							   and b = (galculator e2) in
							   if (a-.b < 0.1 && a-.b > -0.1) then 0.0
							   else if (a>b) then (-.(galculator(INTEGRAL(e2, e1, e3))))
							   else (0.1 *. galculator_sub(e3, a)) +. galculator (INTEGRAL (REAL (a+.0.1), REAL b, e3))

