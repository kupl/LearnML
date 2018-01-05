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

 
let rec galculator_integral (v1, v2, e) = 
	if abs_float (v1 -. v2) < 0.1 then 0.
	else if (v1 > v2) then -. galculator_integral (v2, v1, e)
	else galculator_integral (v1 +. 0.1, v2, e) +. 0.1 *. galculator_variable (v1, e)
and
galculator_variable (v, e) = 
	match e with
	| X -> v
	| INT x -> (float_of_int x)
	| REAL x -> x
	| ADD (x, y) -> (galculator_variable (v, x)) +. (galculator_variable (v, y))
	| SUB (x, y) -> (galculator_variable (v, x)) -. (galculator_variable (v, y))
	| MUL (x, y) -> (galculator_variable (v, x)) *. (galculator_variable (v, y))
	| DIV (x, y) -> (galculator_variable (v, x)) /. (galculator_variable (v, y))
	| INTEGRAL (x, y, z) -> galculator_integral ((galculator x), (galculator y), z)
	| SIGMA (x, y, z) -> galculator_sigma (galculator_variable (v, x), galculator_variable (v, y), z)
and
galculator_sigma (v1, v2, e) = 
	let value1 = (int_of_float v1) in let value2 = (int_of_float v2) in
	let sum = ref 0. in for i = value1 to value2 do sum := !sum +. galculator_variable ((float_of_int i), e) done; !sum
and
galculator e = 
	match e with
	| X -> raise FreeVariable
	| INT e1 -> (float_of_int e1)
	| REAL e1 -> e1
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, e3) -> galculator_sigma ((galculator e1), (galculator e2), e3)
	| INTEGRAL (e1, e2, e3) -> galculator_integral ((galculator e1), (galculator e2), e3)
