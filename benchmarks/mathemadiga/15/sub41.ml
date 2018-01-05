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

let rec gal (isBound, e, k) : float =
	match e with
	| X -> if isBound = true then k else raise FreeVariable
	| INT x -> float_of_int x
	| REAL x -> x
	| ADD(x, y) -> gal(isBound, x, k) +. gal (isBound, y, k)
	| SUB(x, y) -> gal(isBound, x, k) -. gal (isBound, y, k)
	| MUL(x, y) -> gal(isBound, x, k) *. gal (isBound, y, k)
	| DIV(x, y) -> gal(isBound, x, k) /. gal (isBound, y, k)
  | SIGMA(i, n, f) -> 
		let lb = int_of_float (gal (isBound, i, k)) in
		let ub = int_of_float (gal (isBound, n, k)) in
		if lb = ub then gal (true, f, float_of_int lb) 
		else if lb > ub then 0.0
		else gal(true, f, float_of_int lb) +. gal(isBound, SIGMA(ADD(INT lb, INT 1), INT ub, f), k)
	(*| SIGMA(i, n, f) -> 
		let lb = int_of_float (gal (isBound, i, k)) in
		let ub = int_of_float (gal (isBound, n, k)) in
		if lb = ub then gal (true, f, float_of_int lb) 
		else if lb > ub then 0.0
		else gal(true, f, float_of_int lb) +. gal(isBound, SIGMA(ADD(REAL (float_of_int lb), INT 1), REAL (float_of_int ub), f), k)*)
	| INTEGRAL(a, b, f) ->
		let al = gal(isBound, a, k) in
		let be = gal(isBound, b, k) in
		if al > be then
			if al-.be < 0.1 then 0.0
			else (-1.0)*.(gal (isBound, INTEGRAL(REAL be, REAL al, f), k))
		else if be -. al < 0.1 then 0.0
		else 
			(gal(true, f, al))*.(0.1) +. gal(isBound, INTEGRAL(ADD(REAL al, REAL 0.1), REAL be, f), k)
			
			
let galculator e : float = 
	gal (false, e, -1.0)
	

