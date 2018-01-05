type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

type environment = NONE | ENV of float

exception FreeVariable

let absdiff : (float * float) -> float
 = fun (a, b) -> if (a > b) then (a -. b) else (b -. a)

let rec sigma (a, b, expr) =
	if (a > b) then 0.
	else (calcGalculator (expr, ENV (float_of_int a)) +. sigma(a + 1, b, expr))

and integral (a, b, expr) =
	if (absdiff(a, b) < 0.1) then 0.
	else if (a > b) then (integral(b, a, expr)) *. -1.
	else (calcGalculator (expr, ENV a)) *. 0.1 +. integral(a +. 0.1, b, expr)

and calcGalculator (expr, env) =
		match expr with
		| X -> 
			(
				match env with 
				| NONE -> raise FreeVariable
				| ENV r -> r
			)
		| INT i -> float_of_int i
		| REAL r -> r
		| ADD (exp1, exp2) -> (calcGalculator (exp1, env)) +. (calcGalculator (exp2, env))
		| SUB (exp1, exp2) -> (calcGalculator (exp1, env)) -. (calcGalculator (exp2, env))
		| MUL (exp1, exp2) -> (calcGalculator (exp1, env)) *. (calcGalculator (exp2, env))
		| DIV (exp1, exp2) -> (calcGalculator (exp1, env)) /. (calcGalculator (exp2, env))
		| SIGMA (low, high, subExpr) -> sigma (int_of_float(calcGalculator (low, env)), int_of_float(calcGalculator (high, env)), subExpr)
		| INTEGRAL (low, high, subExpr) -> integral (calcGalculator (low, env), calcGalculator (high, env), subExpr)

let galculator: exp -> float 
	= fun expr -> calcGalculator(expr, NONE)
