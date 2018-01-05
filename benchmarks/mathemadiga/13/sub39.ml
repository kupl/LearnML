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

let galculator = fun g_input ->
	let rec gal_x = fun x_input x_now ->
		match x_input with
			X -> if x_now <> x_now then raise FreeVariable else x_now
			| INT(i) -> (float_of_int i)
			| REAL(i) -> i
			| ADD(i1, i2) -> (gal_x i1 x_now) +. (gal_x i2 x_now)
			| SUB(i1, i2) -> (gal_x i1 x_now) -. (gal_x i2 x_now)
			| MUL(i1, i2) -> (gal_x i1 x_now) *. (gal_x i2 x_now)
			| DIV(i1, i2) -> (gal_x i1 x_now) /. (gal_x i2 x_now)
			| SIGMA(i1, i2, i3) -> if (int_of_float (gal_x i1 x_now)) > (int_of_float (gal_x i2 x_now))
														then 0.0
														else (gal_x i3 (gal_x i1 x_now)) +. (gal_x (SIGMA ((REAL ((gal_x i1 x_now) +. 1.0)), i2, i3)) x_now)
			| INTEGRAL(i1, i2, i3) -> if (gal_x i1 x_now) -. (gal_x i2 x_now) >= 0.1
															then 0. -. (gal_x (INTEGRAL (i2, i1, i3)) x_now)
														else if(gal_x i2 x_now) -. (gal_x i1 x_now) >= 0.1
															then 0.1 *. (gal_x i3 (gal_x i1 x_now)) +. (gal_x (INTEGRAL (REAL ((gal_x i1 x_now) +. 0.1), i2, i3)) x_now)
														else 0.0
	in gal_x g_input nan
