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

let galculator (expr: exp) : float =
	let rec galc (e: exp) (xval: float option) : float =
		match e with
		| X -> (match xval with None -> raise FreeVariable | Some n -> n)
		| INT i -> float_of_int i
		| REAL f -> f
		| ADD (e1, e2) -> (galc e1 xval) +. (galc e2 xval)
		| SUB (e1, e2) -> (galc e1 xval) -. (galc e2 xval)
		| MUL (e1, e2) -> (galc e1 xval) *. (galc e2 xval)
		| DIV (e1, e2) -> (galc e1 xval) /. (galc e2 xval)
		| SIGMA (e1, e2, e3) ->
			let rec sigma (l : int) (u : int) (e : exp) (r : float) : float =
				if l > u then r
				else sigma (l + 1) u e (r +. (galc e (Some (float_of_int l))))
			and lower = int_of_float (galc e1 xval)
			and upper = int_of_float (galc e2 xval) in
			sigma lower upper e3 0.0
		| INTEGRAL (e1, e2, e3) ->
			let rec integral (l : float) (u : float) (e : exp) (r : float) : float =
				if l +. 0.1 > u then r
				else integral (l +. 0.1) u e (r +. 0.1 *. (galc e (Some l)))
			and lower = (galc e1 xval)
			and upper = (galc e2 xval) in
			if lower > upper then -. (integral upper lower e3 0.0)
			else integral lower upper e3 0.0
	in
	galc expr None

