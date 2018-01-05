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

let rec sigma (x, y, z) =
	let ix = int_of_float(galculator(x)) in
	let iy = int_of_float(galculator(y)) in
	if ix > iy then INT 0
	else ADD(env(z, y), sigma(x, SUB(y, INT 1), z))
and checkInterval (x, y, z) =
	let ix=galculator(x) in
	let iy=galculator(y) in
	if ix > iy then MUL(INT (-1), INTEGRAL(y, x, z))
	else integral(x, y, z)
and integral (x, y, z) =
	let ix=galculator(x) in
	let iy=galculator(y) in
	if ix+.0.01 > iy then INT 0
	else ADD(MUL(env(z, x), REAL 0.1), integral(ADD(x, REAL 0.1), y, z))
and galculator exp =
	match exp with
	| X -> raise FreeVariable
	| INT x -> float_of_int(x)
	| REAL x -> x
	| ADD (x, y) -> galculator(x) +. galculator(y)
	| SUB (x, y) -> galculator(x) -. galculator(y)
	| MUL (x, y) -> galculator(x) *. galculator(y)
	| DIV (x, y) -> galculator(x) /. galculator(y)
	| SIGMA (x, y, z) -> galculator(sigma(x, y, z))
	| INTEGRAL (x, y, z) -> galculator(checkInterval(x, y, z))
and env (exp, xvalue) =
	match exp with
	| X -> xvalue
	| ADD (x, y) -> ADD(env(x, xvalue),env(y, xvalue))
	| SUB (x, y) -> SUB(env(x, xvalue),env(y, xvalue))
	| MUL (x, y) -> MUL(env(x, xvalue),env(y, xvalue))
	| DIV (x, y) -> DIV(env(x, xvalue),env(y, xvalue))
	| SIGMA (x, y, z) -> sigma(x, y, z)
	| INTEGRAL (x, y, z) -> checkInterval(x, y, z)
	| _ -> exp