type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

type var = UNDEF
	| DEF of float

exception FreeVariable

let rec gal(f, k) = 
        match f with
	| X -> (
		match k with
		| UNDEF -> raise FreeVariable
		| DEF(r) -> r
		)
        | INT(x) -> float_of_int(x)
        | REAL(x) -> x
        | ADD(x, y) -> gal(x, k) +. gal(y, k)
        | SUB(x, y) -> gal(x, k) -. gal(y, k)
        | MUL(x, y) -> gal(x, k) *. gal(y, k)
        | DIV(x, y) -> gal(x, k) /. gal(y, k)
        | SIGMA(st, ed, formula) -> (
                if (int_of_float(gal(st, k)) <= int_of_float(gal(ed, k))) then
                        gal(formula, DEF(float_of_int(int_of_float(gal(st, k))))) +. gal(SIGMA(INT(int_of_float(gal(st, k)) + 1), INT(int_of_float(gal(ed, k))), formula), k)
		else
			0.
                )
	| INTEGRAL(st, ed, formula) -> (
		if (gal(st, k) > gal(ed, k)) then -1.0 *. gal(INTEGRAL(ed, st, formula), k)
                else if (gal(st, k) <= gal(ed, k) -. 0.1) then
                        (gal(formula, DEF(gal(st, k))) *. 0.1) +. gal(INTEGRAL(REAL(gal(st, k) +. 0.1), REAL(gal(ed, k)), formula), k)
                else
                        0.
                )

let galculator(expr) = 
	gal(expr, UNDEF)
