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

let rec substitute a b=
	match b with
	| X -> REAL a
	| INT i -> INT i
	| REAL r -> REAL r
	| ADD (e1,e2) -> ADD ((substitute a e1),(substitute a e2)) 
	| SUB (e1,e2) -> SUB ((substitute a e1),(substitute a e2))
	| MUL (e1,e2) -> MUL ((substitute a e1),(substitute a e2))
	| DIV (e1,e2) -> DIV ((substitute a e1),(substitute a e2))
	| SIGMA (e1,e2,e3) -> SIGMA ((substitute a e1), (substitute a e2),
			(substitute a e3))
	| INTEGRAL (e1,e2,e3) -> INTEGRAL ((substitute a e1), (substitute a e2),
			(substitute a e3))

 
let rec galculator e=
	match e with
	| X -> raise FreeVariable
	| INT a -> float_of_int a
	| REAL r -> r
	| ADD (e1,e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1,e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1,e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1,e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1,e2,e3) -> if (galculator e1)>(galculator e2) then 0.0
						else galculator (substitute (float_of_int (int_of_float (galculator
											e1))) e3)+.galculator (SIGMA
									(ADD(e1,REAL 1.0),e2,e3))
	| INTEGRAL (e1,e2,e3) -> if (galculator e1)>(galculator e2) then
	(0.0-.galculator(INTEGRAL(e2,e1,e3)))
			else if (galculator e1)+.0.1>(galculator e2) then 0.0
			else galculator (MUL(REAL (galculator (substitute (galculator e1) e3)),REAL 0.1))+.galculator
(INTEGRAL(ADD(e1,REAL 0.1),e2,e3))


