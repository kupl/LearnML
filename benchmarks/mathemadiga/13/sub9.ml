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

let rec galculator e list = 
	match e with
	| X ->
		if List.length list < 1 then raise FreeVariable
		else List.hd list
	| INT e -> float_of_int e
	| REAL e -> e
	| ADD (e1, e2) -> (galculator e1 list) +. (galculator e2 list)
	| SUB (e1, e2) -> (galculator e1 list) -. (galculator e2 list)
	| MUL (e1, e2) -> (galculator e1 list) *. (galculator e2 list)
	| DIV (e1, e2) -> (galculator e1 list) /. (galculator e2 list)
	| SIGMA (e1, e2, e3) ->
		let a = int_of_float (galculator e1 list) in
		let b = int_of_float (galculator e2 list) in
		if a < b
			then (galculator (SIGMA(INT a, INT a, e3)) list) +. (galculator (SIGMA(INT (a + 1), INT b, e3)) list)
		else if a == b
			then galculator e3 [float_of_int a]
		else 0.0
	| INTEGRAL (e1, e2, e3) ->
		let a = (galculator e1 list) in
		let b = (galculator e2 list) in
		if a < b -. 0.1
			then (galculator (INTEGRAL(REAL a, REAL a, e3)) list) +. (galculator (INTEGRAL(REAL (a +. 0.1), REAL b, e3)) list)
		else if a == b
			then (galculator e3 [a]) *. 0.1
		else
			if (a > b) && (a -. b > 0.1)
				then (galculator (INTEGRAL(e2, e1, e3)) list) *. -1.0
			else
				0.0

let galculator e =
	galculator e []
			

			
	