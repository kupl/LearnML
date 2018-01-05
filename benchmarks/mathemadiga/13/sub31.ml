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

let galculator expr =
	let rec subgalculator expr condition = 
 		match expr with
			X -> (
				if (condition = []) then raise FreeVariable
				else (List.hd condition) )   
			|INT(i) -> (float_of_int i)
			| REAL(f) -> f
			| ADD(e1, e2) -> ((subgalculator e1 condition) +. (subgalculator e2 condition)) 
			| SUB(e1, e2) -> ((subgalculator e1 condition) -. (subgalculator e2 condition))
			| MUL(e1, e2) -> ((subgalculator e1 condition) *. (subgalculator e2 condition))
			| DIV(e1, e2) -> ((subgalculator e1 condition) /. (subgalculator e2 condition))
			| SIGMA(e1, e2, e3) -> (
				if ((int_of_float (subgalculator e1 condition)) > (int_of_float (subgalculator e2 condition))) then 0.0
				else (subgalculator e3 ((float_of_int (int_of_float (subgalculator e1 condition)))::condition)) +. (subgalculator (SIGMA(ADD(e1, INT(1)), e2, e3)) condition) )
			| INTEGRAL(e1, e2, e3) -> (
				if ((subgalculator e1 condition) <= (subgalculator e2 condition)) then (
					if (((subgalculator e2 condition) -. (subgalculator e1 condition) < 0.1)) then 0.0
					else ((subgalculator (MUL(e3, REAL(0.1))) ((subgalculator e1 condition)::condition)) +. (subgalculator (INTEGRAL(ADD(e1, REAL(0.1)), e2, e3)) condition)) )
				else (0.0 -. (subgalculator (INTEGRAL(e2, e1, e3)) condition)) )
	in (subgalculator expr [])
