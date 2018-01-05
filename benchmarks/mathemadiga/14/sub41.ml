
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

let rec galculator (e : exp) : float =
	
	let rec xCal (ex : exp) (x : float) : float =
		match ex with
		| X -> x
		| INT i -> float_of_int i
		| REAL f -> f
		| ADD (e1, e2) -> (xCal e1 x) +. (xCal e2 x)
		| SUB (e1, e2) -> (xCal e1 x) -. (xCal e2 x)
		| MUL (e1, e2) -> (xCal e1 x) *. (xCal e2 x)
		| DIV (e1, e2) -> (xCal e1 x) /. (xCal e2 x)
		| SIGMA (e1, e2, e3) -> let a = int_of_float (xCal e1 x) in
					let b = int_of_float (xCal e2 x) in
					if (a > b) then 0.0
					else (xCal e3 (float_of_int a)) +. (xCal (SIGMA (ADD (e1, INT 1), e2, e3)) x)
		| INTEGRAL (e1, e2, e3) -> let a = (xCal e1 x) in
					   let b = (xCal e2 x) in
					   if (abs_float (a-.b)) < 0.1 then 0.0
					   else if (a < b) then (0.1 *. (xCal e3 a)) +. (xCal (INTEGRAL (ADD (e1, REAL 0.1), e2, e3)) x)
					   else -. (xCal (INTEGRAL (e2, e1, e3)) x); in
	
	match e with
	| X -> raise FreeVariable
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, e3) -> let a = int_of_float (galculator e1) in
				let b = int_of_float (galculator e2) in
				if (a > b) then 0.0
				else (xCal e3 (float_of_int a)) +. (galculator (SIGMA (ADD (INT a, INT 1), REAL (float_of_int b), e3)))
	| INTEGRAL (e1, e2, e3) -> let a = galculator e1 in
				   let b = galculator e2 in
				   if (abs_float (a-.b)) < 0.1 then 0.0
				   else if (a < b) then (0.1 *. (xCal e3 a)) +. (galculator (INTEGRAL (ADD (e1, REAL 0.1), e2, e3)))
				   else -. (galculator (INTEGRAL (e2, e1, e3)))
