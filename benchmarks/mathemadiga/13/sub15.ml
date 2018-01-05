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

let rec galculator a =
	let rec realg a b v =
		match a with
		| X -> if b then v
			else raise FreeVariable
		| INT x -> float_of_int x
		| REAL x -> x
		| ADD (x, y) -> (realg x b v) +. (realg y b v)
		| SUB (x, y) -> (realg x b v) -. (realg y b v)
		| MUL (x, y) -> (realg x b v) *. (realg y b v)
		| DIV (x, y) -> (realg x b v) /. (realg y b v)
		| SIGMA (x, y, z) -> if (realg x b v) > (realg y b v) then 0.0
					else (realg z true (galculator (INT (int_of_float (realg x b v))))) +. (realg (SIGMA (ADD(x,REAL 1.0), y, z)) b v)
		| INTEGRAL (x, y, z) -> if (realg x b v) > (realg y b v) then realg (SUB (REAL 0., INTEGRAL (y, x, z))) b v
					else if (realg y b v) -. (realg x b v) < 0.1  then 0.0
					else ((realg z true (realg x b v)) *. 0.1) +. (realg (INTEGRAL (ADD(x,REAL 0.1), y, z)) b v)
	in
	realg a false 0.0 
