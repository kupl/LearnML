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

let rec xcalculator exp value =
	match exp with
	| X -> value
	| INT n -> float_of_int n
	| REAL r -> r
	| ADD(a, b) -> (xcalculator a value) +. (xcalculator b value)
	| SUB(a, b) -> (xcalculator a value) -. (xcalculator b value)
	| MUL(a, b) -> (xcalculator a value) *. (xcalculator b value)
	| DIV(a, b) -> (xcalculator a value) /. (xcalculator b value)
	| SIGMA(lower, upper, f) ->
		if int_of_float (xcalculator lower value) > int_of_float (xcalculator upper value) then 0.
		else xcalculator f (float_of_int (int_of_float (xcalculator lower value))) +. xcalculator (SIGMA(ADD(lower, INT 1), upper, f)) value
	| INTEGRAL(lower, upper, f) ->
		if xcalculator lower value > xcalculator upper value then xcalculator (INTEGRAL(upper, lower, f)) value *. -1.
		else if xcalculator lower value +. 0.1 > xcalculator upper value then 0.
		else xcalculator f (xcalculator lower value) *. 0.1 +. xcalculator (INTEGRAL(ADD(lower, REAL 0.1), upper, f)) value

let rec galculator exp =
	match exp with
	| X -> raise FreeVariable
	| INT n -> float_of_int n
	| REAL r -> r
	| ADD(a, b) -> (galculator a) +. (galculator b)
	| SUB(a, b) -> (galculator a) -. (galculator b)
	| MUL(a, b) -> (galculator a) *. (galculator b)
	| DIV(a, b) -> (galculator a) /. (galculator b)
	| SIGMA(lower, upper, f) ->
		if int_of_float (galculator lower) > int_of_float (galculator upper) then 0.
		else xcalculator f (float_of_int (int_of_float (galculator lower))) +. galculator (SIGMA(ADD(lower, INT 1), upper, f))
	| INTEGRAL(lower, upper, f) ->
		if galculator lower > galculator upper then galculator (INTEGRAL(upper, lower, f)) *. -1.
		else if galculator lower +. 0.1 > galculator upper then 0.
		else xcalculator f (galculator lower) *. 0.1 +. galculator (INTEGRAL(ADD(lower, REAL 0.1), upper, f))
