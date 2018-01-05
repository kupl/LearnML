exception FreeVariable

type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp;;

let rec galculator = function
| X -> raise (FreeVariable)
| INT x -> float_of_int x
| REAL x -> x
| ADD (x, y) -> galculator(x) +. galculator(y)
| SUB (x, y) -> galculator(x) -. galculator(y)
| MUL (x, y) -> galculator(x) *. galculator(y)
| DIV (x, y) -> galculator(x) /. galculator(y)
| SIGMA (x, y, z) -> sigma(int_of_float (galculator(x)), int_of_float (galculator(y)), z)
| INTEGRAL (x, y, z) -> integral(galculator(x), galculator(y), z)

and sigma(x, y, z) =
	if x > y then 0.
	else if x = y then evalexp(z, float_of_int x)
	else sigma(x, (x + y) / 2, z) +. sigma((x + y) / 2 + 1, y, z)

and integral(x, y, z) =
	let rec aux (x, y, z, sum) =
		if x > y then 0. -. aux(y, x, z, sum)
		else if (y -. x) < 0.1 then sum
		else aux(x +. 0.1, y, z, sum +. evalexp(z, x) *. 0.1) in
	aux(x, y, z, 0.)

and evalexp (input, num) = 
	match input with
	| X -> num
	| INT x -> float_of_int x
	| REAL x -> x
	| ADD (x, y) -> evalexp(x, num) +. evalexp(y, num)
	| SUB (x, y) -> evalexp(x, num) -. evalexp(y, num)
	| MUL (x, y) -> evalexp(x, num) *. evalexp(y, num)
	| DIV (x, y) -> evalexp(x, num) /. evalexp(y, num)
	| SIGMA (x, y, z) -> sigma(int_of_float (evalexp(x, num)), int_of_float (evalexp(y, num)), z)
	| INTEGRAL (x, y, z) -> integral(evalexp(x, num), evalexp(y, num), z);;

