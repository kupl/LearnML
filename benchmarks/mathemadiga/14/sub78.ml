type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp;;

exception FreeVariable;;

let sigma_d (f1, f2, f3) =
	let i1 = int_of_float f1 in
	let i2 = int_of_float f2 in
	if i1 > i2 then 0. else (float (i2 - i1 + 1 )) *. f3;;

let rec assign exp x = match exp with
| X -> x
| INT i -> float i
| REAL f -> f
| ADD(e1, e2) -> (assign e1 x) +. (assign e2 x)
| SUB(e1, e2) -> (assign e1 x) -. (assign e2 x)
| MUL(e1, e2) -> (assign e1 x) *. (assign e2 x)
| DIV(e1, e2) -> (assign e1 x) /. (assign e2 x)
| SIGMA(e1, e2, e3) -> sigma_d(assign e1 x, assign e2 x, assign e3 x)
| INTEGRAL(e1, e2, e3) -> ((assign e2 x) -. (assign e1 x)) *. (assign e3 x)
;;

let rec sigma (i1, i2, e) = 
	if i1 > i2 then 0.
	else sigma(i1, i2-1, e) +. (assign e (float i2));;

let rec integral(f1, f2, e) = 
	if f1 > f2 then -. integral(f2, f1, e)
	else if (f2 -. f1) < 0.1 then 0.0
	else integral(f1 +. 0.1, f2, e) +. (assign e f1) *. 0.1;;

let rec galculator = function
| X -> raise FreeVariable
| INT i -> float i
| REAL f -> f
| ADD(e1, e2) -> galculator(e1) +. galculator(e2)
| SUB(e1, e2) -> galculator(e1) -. galculator(e2)
| MUL(e1, e2) -> galculator(e1) *. galculator(e2)
| DIV(e1, e2) -> galculator(e1) /. galculator(e2)
| SIGMA(e1, e2, e) ->
	let a = int_of_float(galculator e1) in
	let b = int_of_float(galculator e2) in
	sigma(a,b,e)
| INTEGRAL(e1, e2, e) -> integral(galculator e1, galculator e2, e);;