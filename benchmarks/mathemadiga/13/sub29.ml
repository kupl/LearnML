type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception FreeVariable

let rec insert (exp, const) =
	match (exp, const) with
	| (X, const) -> REAL const
	| (INT a, const) -> INT a
	| (REAL a, const) -> REAL a
	| (ADD(e1, e2), const) -> ADD(insert(e1, const), insert(e2, const))
	| (SUB(e1, e2), const) -> SUB(insert(e1, const), insert(e2, const))
	| (MUL(e1, e2), const) -> MUL(insert(e1, const), insert(e2, const))
	| (DIV(e1, e2), const) -> DIV(insert(e1, const), insert(e2, const))
	| (SIGMA(s, e, exp), const) -> SIGMA(insert(s, const), insert(e, const), exp)
	| (INTEGRAL(s, e, exp), const) -> INTEGRAL(insert(s, const), insert(e, const), exp)

let rec galculator e =
	match e with
	| X -> raise FreeVariable
	| INT a -> float_of_int a
	| REAL b -> b
	| ADD (a, b) -> galculator a +. galculator b
	| SUB (a, b) -> galculator a -. galculator b
	| MUL (a, b) -> galculator a *. galculator b
	| DIV (a, b) -> galculator a /. galculator b
	| SIGMA (a, b, c) -> if (galculator a > galculator b) then 0.
			     else galculator(insert(c, float_of_int(int_of_float(galculator a)))) +. galculator(SIGMA(ADD(a, INT 1), b, c))
	| INTEGRAL (a, b, c) -> if (abs_float(galculator a -. galculator b) < 0.1) then 0.
				else if (galculator a < galculator b) then galculator(MUL(insert(c, galculator a), REAL 0.1)) +. galculator(INTEGRAL(ADD(a, REAL 0.1), b, c))
				else galculator(SUB(REAL 0., INTEGRAL(b, a, c)))
