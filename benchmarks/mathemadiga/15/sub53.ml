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

type arg = NOT | HAVE of float

let rec galculator exp =
	let rec make_fun exp arg =
			match exp with
			| INT i -> float_of_int i
			| REAL f -> f
			| X ->
				(match arg with
				| NOT -> raise FreeVariable
				| HAVE f -> f)
			| ADD (e1, e2) -> (make_fun e1 arg) +. (make_fun e2 arg)
			| MUL (e1, e2) -> (make_fun e1 arg) *. (make_fun e2 arg)
			| SUB (e1, e2) -> (make_fun e1 arg) -. (make_fun e2 arg)
			| DIV (e1, e2) -> (make_fun e1 arg) /. (make_fun e2 arg)
			| SIGMA (e1, e2, e3) ->
				let a = make_fun e1 arg
				and b = make_fun e2 arg in
				if(a>b) then 0.0
				else
					let rec sigma a b exp =
					if(a>b) then 0.0
					else
					(make_fun exp (HAVE (float_of_int a))) +. (sigma (a+1) b exp) in
						sigma (int_of_float a) (int_of_float b) e3
			| INTEGRAL (e1, e2, e3) ->
				let a = make_fun e1 arg
				and b = make_fun e2 arg in
				let rec integral a b exp =
					if(a>b+.0.1) then -1. *. (integral b a exp)
					else if ((a >= b) && (a<b +. 0.1)) then 0.0
					else ((make_fun exp (HAVE a))*.0.1) +. (integral (a+.0.1) b exp) in
						integral a b e3
		in
		make_fun exp NOT
