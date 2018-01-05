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

let rec calc e x =
	match (e, x) with
	| (X, x) -> x
	| (INT i, x) -> float_of_int i
	| (REAL f, x) -> f
	| (ADD (e1, e2), x) -> (calc e1 x) +. (calc e2 x)
	| (SUB (e1, e2), x) -> (calc e1 x) -. (calc e2 x)
	| (MUL (e1, e2), x) -> (calc e1 x) *. (calc e2 x)
	| (DIV (e1, e2), x) -> (calc e1 x) /. (calc e2 x)
	| (SIGMA (e1, e2, e3), x) ->
		let x2 = (int_of_float (calc e1 x)) in
		let y2 = (int_of_float (calc e2 x)) in
		if x2 = y2 then (calc e3 (float_of_int x2))
		else if x2 > y2 then 0.0
		else (calc e3 (float_of_int x2)) +. (calc (SIGMA (INT (x2 + 1), INT y2, e3)) x)
	| (INTEGRAL (e1, e2, e3), x) ->
		let x2 = (calc e1 x) in
		let y2 = (calc e2 x) in
		if (-. 0.1 < (x2 -. y2)) && ((x2 -. y2) < 0.1) then 0.0
		else if e1 > e2 then -. (calc (INTEGRAL (e2, e1, e3)) x)
		else ((calc e3 x2) *. 0.1) +. (calc (INTEGRAL (REAL (x2 +. 0.1), REAL y2, e3)) x)

let rec galculator e =
	match e with
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, e3) ->
		let x = (int_of_float (galculator e1)) in
		let y = (int_of_float (galculator e2)) in
		if x = y then (calc e3 (float_of_int x))
		else if x > y then 0.0
		else (calc e3 (float_of_int x)) +. (galculator (SIGMA (INT (x + 1), e2, e3)))
	| INTEGRAL (e1, e2, e3) ->
		let x = (galculator e1) in
		let y = (galculator e2) in
		if (-. 0.1 < (x -. y)) && ((x -. y) < 0.1) then 0.0
		else if e1 > e2 then -. (galculator (INTEGRAL (e2, e1, e3)))
		else ((calc e3 x) *. 0.1) +. (galculator (INTEGRAL (REAL (x +. 0.1), e2, e3)))
	| _ -> raise FreeVariable
		
	
		 
