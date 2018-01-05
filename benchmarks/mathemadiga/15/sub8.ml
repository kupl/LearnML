exception FreeVariable

type exp =	X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

let rec galculator x =
	(* Check if it used as a function *)
	let rec hasX e =
		match e with
		| X -> true
		| INT n -> false
		| REAL f -> false
		| ADD (e1, e2) -> (hasX e1) || (hasX e2)
		| SUB (e1, e2) -> (hasX e1) || (hasX e2)
		| MUL (e1, e2) -> (hasX e1) || (hasX e2)
		| DIV (e1, e2) -> (hasX e1) || (hasX e2)
		| SIGMA (e1, e2, e3) -> (hasX e1) || (hasX e2)
		| INTEGRAL (e1, e2, e3) -> (hasX e1) || (hasX e2) in
	(* Input the value in case when it is used as a function *)
	let rec input_value (x, y) =
		match x with
		| X -> y
		| INT n -> x
		| REAL f -> x
		| ADD (a, b) -> ADD (input_value (a, y), input_value (b, y))
		| SUB (a, b) -> SUB (input_value (a, y), input_value (b, y))
		| MUL (a, b) -> MUL (input_value (a, y), input_value (b, y))
		| DIV (a, b) -> DIV (input_value (a, y), input_value (b, y))
		| SIGMA (a, b, c) -> SIGMA (input_value (a, y), input_value (b, y), c)
		| INTEGRAL (a, b, c) -> INTEGRAL (input_value (a, y), input_value (b, y), c) in
	match x with
	| X -> raise FreeVariable
	| INT n -> float_of_int n
	| REAL f -> f
	| ADD (a, b) -> (galculator a) +. (galculator b)
	| SUB (a, b) -> (galculator a) -. (galculator b)
	| MUL (a, b) -> (galculator a) *. (galculator b)
	| DIV (a, b) -> (galculator a) /. (galculator b)
	| SIGMA (x1, x2, f) -> if (hasX f) = false then (float_of_int ((int_of_float (galculator x2)) - (int_of_float (galculator x1)))) *. (galculator f)
			       else if (galculator x1) > (galculator x2) then 0.0
			       else if (galculator x1) = (galculator x2) then galculator (input_value (f, REAL (float_of_int (int_of_float (galculator x1)))))
			       else
				    (galculator (input_value (f, (REAL (float_of_int (int_of_float (galculator x1))))))) +. (galculator (SIGMA ((ADD (x1, (INT 1))), x2, f)))
	| INTEGRAL (x1, x2, f) -> if (galculator x1) > (galculator x2) then -1.0 *. (galculator (INTEGRAL (x2, x1, f)))
				  else if (galculator x2) -. (galculator x1) < 0.1 then 0.0
				  else ((galculator (input_value (f, REAL (galculator x1)))) *. 0.1) +. (galculator (INTEGRAL ((ADD (x1, (REAL 0.1))), x2, f)))

