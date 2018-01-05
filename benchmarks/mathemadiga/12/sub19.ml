type exp =
	  X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception FreeVariable

let mathemadiga exp =
	let rec mathe (x, e) = match e with
	| X -> (match x with
		| None -> raise FreeVariable
		| Some x'-> x')
	| INT i -> float_of_int i
	| REAL r -> r
	| ADD (e1, e2) -> (mathe (x, e1)) +. (mathe (x, e2))
	| SUB (e1, e2) -> (mathe (x, e1)) -. (mathe (x, e2))
	| MUL (e1, e2) -> (mathe (x, e1)) *. (mathe (x, e2))
	| DIV (e1, e2) -> (mathe (x, e1)) /. (mathe (x, e2))
	| SIGMA (a, b, f) ->
		let a' = int_of_float (mathe (x, a)) in
		let b' = int_of_float (mathe (x, b)) in
		if a' > b' then 0.0
		else if a' = b' then mathe (Some (float_of_int a'), f)
		else (mathe (Some (float_of_int a'), f)) +. mathe (x, SIGMA (INT (a' + 1), INT b', f))
	| INTEGRAL (a, b, f) ->
		let a' = mathe (x, a) in
		let b' = mathe (x, b) in
		if a' > b' then -.(mathe (x, INTEGRAL (REAL b', REAL a', f)))
		else if b' -. a' <= 0.1 then 0.1 *. mathe (Some a', f)
		else (0.1 *. (mathe (Some a', f))) +. (mathe (x, INTEGRAL (REAL (a' +. 0.1), REAL b', f)))
	in
	mathe (None, exp)