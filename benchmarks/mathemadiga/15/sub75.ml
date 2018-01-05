type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp;;

exception FreeVariable

let rec galculator e =
	match e with
	| X -> raise FreeVariable
	| INT n -> float n
	| REAL x -> x
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, e3) -> sigma (int_of_float (galculator e1)) (int_of_float (galculator e2)) e3
	| INTEGRAL (e1, e2, e3) -> integral (galculator e1) (galculator e2) e3

and sigma a b e =
	if a > b then 0.
	else (galc e (float a)) +. (sigma (a+1) b e)

and integral a b e =
	if a > b then 0.-.(integral b a e)
	else if a +. 0.1 > b then 0.
	else ((galc e a)*.0.1) +. (integral (a+.0.1) b e)

and galc e value =
	match e with
	| X -> value
	| INT n -> float n
	| REAL x -> x
	| ADD (e1, e2) -> (galc e1 value) +. (galc e2 value)
	| SUB (e1, e2) -> (galc e1 value) -. (galc e2 value)
	| MUL (e1, e2) -> (galc e1 value) *. (galc e2 value)
	| DIV (e1, e2) -> (galc e1 value) /. (galc e2 value)
	| SIGMA (e1, e2, e3) -> sigma (int_of_float (galculator e1)) (int_of_float (galculator e2)) e3
	| INTEGRAL (e1, e2, e3) -> integral (galculator e1) (galculator e2) e3;;
