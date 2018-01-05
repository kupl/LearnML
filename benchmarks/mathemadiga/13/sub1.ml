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

let rec sigma (a, b ,f) =
	if a > b then 0.
    else if a = b then f (float_of_int a)
    else (f (float_of_int a)) +. sigma ((a+1), b, f)

let rec measuration_by_division (a, b, dx, f) =
	if abs_float (a -. b) < dx then 0.
	else if a>b then -1. *. measuration_by_division(b, a, dx, f)
	else ((f a) *. dx) +. measuration_by_division(a+.dx, b, dx, f)

let rec function_of_exp e =
	match e with
	| X -> fun x -> x
	| INT d -> fun x -> float_of_int d
	| REAL f -> fun x -> f
	| ADD (el, er) -> fun x -> (function_of_exp el) x +. (function_of_exp er) x
	| SUB (el, er) -> fun x -> (function_of_exp el) x -. (function_of_exp er) x
	| MUL (el, er) -> fun x -> (function_of_exp el) x *. (function_of_exp er) x
	| DIV (el, er) -> fun x -> (function_of_exp el) x /. (function_of_exp er) x
	| SIGMA (e1, e2, e3) -> fun x -> sigma (int_of_float ((function_of_exp e1) x), int_of_float ((function_of_exp e2) x), function_of_exp e3)
	| INTEGRAL (e1, e2, e3) -> fun x -> measuration_by_division ((function_of_exp e1) x, (function_of_exp e2) x, 0.1, function_of_exp e3)

let rec galculator e = 
	match e with
	| X -> raise FreeVariable
	| INT d -> float_of_int d
	| REAL f -> f
	| ADD (el, er) -> galculator el +. galculator er
	| SUB (el, er) -> galculator el -. galculator er
	| MUL (el, er) -> galculator el *. galculator er
	| DIV (el, er) -> galculator el /. galculator er
	| SIGMA (e1, e2, e3) -> sigma (int_of_float (galculator e1), int_of_float(galculator e2), function_of_exp e3)
	| INTEGRAL (e1, e2, e3) -> measuration_by_division (galculator e1, galculator e2, 0.1, function_of_exp e3)