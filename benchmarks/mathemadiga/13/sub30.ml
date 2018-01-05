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

let rec galculator exp =
	match exp with
	| X -> raise FreeVariable
	| INT a -> float_of_int a
	| REAL a -> a
	| ADD (e1, e2) -> galculator e1 +. galculator e2
	| SUB (e1, e2) -> galculator e1 -. galculator e2
	| MUL (e1, e2) -> galculator e1 *. galculator e2
	| DIV (e1, e2) -> galculator e1 /. galculator e2
	| SIGMA (e1, e2, e3) ->
			let rec aux e f =
				match f with
				| X -> e
				| INT a -> INT a
				| REAL a -> REAL a
				| ADD (e1, e2) -> ADD (aux e e1, aux e e2)
				| SUB (e1, e2) -> SUB (aux e e1, aux e e2)
				| MUL (e1, e2) -> MUL (aux e e1, aux e e2)
				| DIV (e1, e2) -> DIV (aux e e1, aux e e2) 
				| SIGMA (e1, e2, e3) -> SIGMA (aux e e1, aux e e2, e3)
				| INTEGRAL (e1, e2, e3) -> INTEGRAL (aux e e1, aux e e2, e3)
			in
			if galculator e1 > galculator e2 then 0.
			else if galculator e1 = galculator e2 then galculator(aux e1 e3)
			else galculator(aux e1 e3) +. galculator(SIGMA(ADD(e1, INT 1), e2, e3))
	| INTEGRAL (e1, e2, e3) ->
			let rec aux e f =
				match f with
				| X -> e
				| INT a -> INT a
				| REAL a -> REAL a
				| ADD (e1, e2) -> ADD (aux e e1, aux e e2)
				| SUB (e1, e2) -> SUB (aux e e1, aux e e2)
				| MUL (e1, e2) -> MUL (aux e e1, aux e e2)
				| DIV (e1, e2) -> DIV (aux e e1, aux e e2) 
				| SIGMA (e1, e2, e3) -> SIGMA (aux e e1, aux e e2, e3)
				| INTEGRAL (e1, e2, e3) -> INTEGRAL (aux e e1, aux e e2, e3)
			in
			if galculator e1 > galculator e2 then 0. -. galculator(INTEGRAL(e2, e1, e3))
			else if (galculator e2) -. (galculator e1) < 0.1 then 0.
			else galculator(aux e1 e3) *. 0.1 +. galculator(INTEGRAL(ADD(e1, REAL 0.1), e2, e3))
