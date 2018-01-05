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

let rec assign x e =
	match e with
		| X			-> x
		| INT n			-> float_of_int n
		| REAL r		-> r
		| ADD (e1, e2)		-> (assign x e1) +. (assign x e2)
		| SUB (e1, e2)		-> (assign x e1) -. (assign x e2)
		| MUL (e1, e2) 		-> (assign x e1) *. (assign x e2)
		| DIV (e1, e2) 		-> (assign x e1) /. (assign x e2)
		| SIGMA (e1, e2, e3)	-> 
			(let a = int_of_float (assign x e1)
			 and b = int_of_float (assign x e2) in
				if a > b then 0.
				else if a = b then assign (float_of_int a) e3
				else assign (float_of_int a) e3 +. assign x (SIGMA (INT (a+1), INT b, e3)))
		

		| INTEGRAL (e1, e2, e3)	->
			(let a = assign x e1
			 and b = assign x e2 in
				if abs_float(a -. b) < 0.1 then 0.
				else if a > b then -. (assign x (INTEGRAL (REAL b, REAL a, e3)))
				else (assign a e3) *. 0.1 +. (assign x (INTEGRAL (REAL (a +. 0.1), REAL b, e3))))

let rec galculator e =
	match e with
		| X 			-> raise FreeVariable
		| INT n 		-> float_of_int n
		| REAL r 		-> r
		| ADD (e1, e2) 		-> galculator e1 +. galculator e2
		| SUB (e1, e2) 		-> galculator e1 -. galculator e2
		| MUL (e1, e2) 	 	-> galculator e1 *. galculator e2
		| DIV (e1, e2) 		-> galculator e1 /. galculator e2
		| SIGMA (e1, e2, e3) 	-> 
			(let a = int_of_float (galculator e1)
			 and b = int_of_float (galculator e2) in
				if a > b then 0.
				else if a = b then assign (float_of_int a) e3
				else assign (float_of_int a) e3 +. galculator (SIGMA (INT (a+1), INT b, e3)))

		| INTEGRAL (e1, e2, e3)	->
			(let a = galculator e1
			 and b = galculator e2 in
				if abs_float(a -. b) < 0.1 then 0.
				else if a > b then -. galculator (INTEGRAL (REAL b, REAL a, e3))
				else (assign a e3) *. 0.1 +. galculator (INTEGRAL (REAL (a +. 0.1), REAL b, e3)))
