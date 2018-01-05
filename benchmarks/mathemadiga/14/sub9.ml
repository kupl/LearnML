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

let rec calc value e = 
	match e with
    | X -> value
	| INT x -> (float_of_int x)
	| REAL x -> x
	| ADD (e1, e2) -> (calc value e1) +. (calc value e2)
	| SUB (e1, e2) -> (calc value e1) -. (calc value e2)
	| MUL (e1, e2) -> (calc value e1) *. (calc value e2)
	| DIV (e1, e2) -> (calc value e1) /. (calc value e2)
	| SIGMA (e1, e2, e3) -> sigma (int_of_float (calc value e1)) (int_of_float (calc value e2)) e3 0.0
	| INTEGRAL (e1, e2, e3) -> integral (calc value e1) (calc value e2) e3 0.0                             

and

sigma a b e acc =
	if a > b then 0.0
	else if a = b then (calc (float_of_int a) e) +. acc
	else (sigma (a+1) b e (acc +. (calc (float_of_int a) e)))
and

integral a b e acc =
	if (abs_float (b -. a)) < 0.1 then acc
	else if a > b then -.(integral b a e acc)
	else integral (a +. 0.1) b e (acc +. (0.1 *. (calc a e)))
		

let rec galculator e = 
	match e with
	| X -> raise FreeVariable
	| INT x -> (float_of_int x)
	| REAL x -> x
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, e3) -> sigma (int_of_float (galculator e1)) (int_of_float (galculator e2)) e3 0.0
	| INTEGRAL (e1, e2, e3) -> integral (galculator e1) (galculator e2) e3 0.0
