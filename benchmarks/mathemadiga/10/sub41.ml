type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreevarError
exception DividedByZero
let mathemadiga e = 
	let rec mathematica e x is_x_effective =
		match e with
		X -> if is_x_effective then x else raise FreevarError
		| INT a -> (float_of_int)a
		| REAL a -> a
		| ADD (e1, e2) -> (mathematica e1 x is_x_effective) 
			+. (mathematica e2 x is_x_effective) 
		| SUB (e1, e2) -> (mathematica e1 x is_x_effective)
			-. (mathematica e2 x is_x_effective)
		| MUL (e1, e2) -> (mathematica e1 x is_x_effective) 
			*. (mathematica e2 x is_x_effective)
		| DIV (e1, e2) -> let me2 = mathematica e2 x is_x_effective in
			if me2 = 0.0 then raise DividedByZero
			else (mathematica e1 x is_x_effective) /. me2
		| SIGMA (INT k, INT n, e3) -> 
			if k <= n then
				(mathematica e3 ((float_of_int)k) true) +.
				(mathematica (SIGMA (INT (k+1), INT n, e3)) x is_x_effective)
			else 0.0
		| INTEGRAL (k_, n_, e3) ->
			let k = (mathematica k_ x is_x_effective) in
			let n = (mathematica n_ x is_x_effective) in
			if k < n then (
				if (n-.k) < 0.1 then (n-.k)*.(mathematica e3 k true)
				else 0.1*.(mathematica e3 k true) +.
					(mathematica (INTEGRAL (REAL (k+.0.1), n_, e3)) 
						x is_x_effective))
			else if k = n then 0.0
			else -.(mathematica (INTEGRAL (n_, k_, e3)) 
						x is_x_effective)
	in
	mathematica e 0.0 false
