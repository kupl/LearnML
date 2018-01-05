type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

(* hw2-4 Galculator. galculator: exp -> float *)
exception FreeVariable
let rec galculator sik =
	let rec galc_withX sik_inner x =
	(* sik_inner : exp, x : float *)
		match sik_inner with
		| X -> x
		| INT i -> (float_of_int i)
		| REAL f -> f
		| ADD (a, b) -> (galc_withX a x) +. (galc_withX b x)
		| SUB (a, b) -> (galc_withX a x) -. (galc_withX b x)
		| MUL (a, b) -> (galc_withX a x) *. (galc_withX b x)
		| DIV (a, b) -> (galc_withX a x) /. (galc_withX b x)
		| SIGMA (a, b, c) -> (galculator (SIGMA (INT (int_of_float (galc_withX a x)), INT (int_of_float (galc_withX b x)), c)))
		| INTEGRAL (a, b, c) -> (galculator (INTEGRAL (REAL (galc_withX a x), REAL (galc_withX b x), c)))

	in

	match sik with
	| X -> raise FreeVariable
	| INT i -> (float_of_int i)
	| REAL f -> f
	| ADD (a, b) -> (galculator a) +. (galculator b)
	| SUB (a, b) -> (galculator a) -. (galculator b)
	| MUL (a, b) -> (galculator a) *. (galculator b)
	| DIV (a, b) -> (galculator a) /. (galculator b)
	| SIGMA (a, b, c) ->(
		match (a, b) with
		| (INT _a, INT _b) -> if(_a > _b) then 0.0
							else (galc_withX c (float_of_int _a)) +. (galculator (SIGMA (INT (_a + 1), b, c)))
		| _ -> (galculator (SIGMA (INT (int_of_float (galculator a)), INT (int_of_float (galculator b)), c))) 
	)
	| INTEGRAL (a, b, c) -> if((galculator a) > (galculator b)) then (0.0 -. (galculator (INTEGRAL (b, a, c))))
						else if((galculator b) -. (galculator a) < 0.1) then 0.0
						else (galc_withX c (galculator a)) *. 0.1 +. (galculator (INTEGRAL ((REAL ((galculator a) +. 0.1)), b, c)))


