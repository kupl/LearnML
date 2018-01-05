type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

type var = VAL of float
	| FREE



exception FreeVariable

let rec galRec (exp, x) =
	let rec sigma (a, b, exp) =
		if a > b then 0.0
		else galRec(exp, VAL (float_of_int a)) +. sigma(a + 1, b, exp) in
	let rec integral (a, b, exp) =
		if a > b then -.integral(b, a, exp)
		else if b -. a < 0.1 then 0.0
		else galRec(exp, VAL a) *. 0.1 +. integral(a +. 0.1, b, exp) in
	match exp with
	X -> (match x with FREE -> raise FreeVariable | VAL f -> f)
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e0,e1) -> galRec(e0, x) +. galRec(e1, x)
	| SUB (e0,e1) -> galRec(e0, x) -. galRec(e1, x)
	| MUL (e0,e1) -> galRec(e0, x) *. galRec(e1, x)
	| DIV (e0,e1) -> galRec(e0, x) /. galRec(e1, x)
	| SIGMA (a, b, e) -> sigma(int_of_float(galRec(a, x)), int_of_float(galRec(b, x)), e)
	| INTEGRAL (a, b, e) -> integral(galRec(a, x), galRec(b, x), e)

let galculator exp = galRec(exp, FREE)
