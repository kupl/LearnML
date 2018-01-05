exception FreeVariable

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let applyX e arg = if (e == X) then (REAL arg) else e


let rec calc e arg =
	match e with
          X -> arg
        | INT f -> float_of_int(f)
	| REAL f -> f
	| ADD(e1, e2) -> calc (applyX e1 arg) arg +. calc (applyX e2 arg) arg
	| SUB(e1, e2) -> calc (applyX e1 arg) arg -. calc (applyX e2 arg) arg
	| MUL(e1, e2) -> calc (applyX e1 arg) arg *. calc (applyX e2 arg) arg
	| DIV(e1, e2) -> calc (applyX e1 arg) arg /. calc (applyX e2 arg) arg
	| SIGMA (a,b,c) ->
		let rec sum(n, p) =
			if (n < p) then (calc c n) +. sum(n +. 1.0, p)
			else 0.0
		in sum(calc a arg, calc b arg)
	| INTEGRAL (a,b,c) -> 
		let rec integral(n, p) =
			if (n < p) then (calc c n) *. 0.1 +. integral(n +. 0.1, p)
			else 0.0
		in integral(calc a arg, calc b arg)
        

let rec galculator e =
	match e with
	  X -> raise FreeVariable
	| INT f -> float_of_int(f)
	| REAL f -> f
	| ADD (e1, e2) -> galculator e1 +. galculator e2
	| SUB (e1, e2) -> galculator e1 -. galculator e2
	| MUL (e1, e2) -> galculator e1 *. galculator e2
	| DIV (e1, e2) -> galculator e1 /. galculator e2
	| SIGMA (a,b,c) ->
		let rec sum(n, p) =
			if (n <= p) then (calc c n) +. sum(n +. 1.0, p)
			else 0.0
		in sum(galculator a,galculator b)
	| INTEGRAL (a,b,c) -> 
		let rec integral(n, p) =
			if (n +. 0.1 < p) then (calc c n) *. 0.1 +. integral(n +. 0.1, p)
			else 0.0
		in let signintegral(s,e2) = if (s <= e2) then integral(s,e2) else integral(e2, s) *. -1.0
		in signintegral(galculator a, galculator b)
