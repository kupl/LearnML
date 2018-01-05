type exp =
	| X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception FreeVariable
exception DividedByZero

let rec mathemadiga exp =
	let rec foo (exp, boolX, valX) =
		match exp with
		| X -> if boolX then valX else raise FreeVariable
		| INT i -> (float_of_int i)
		| REAL f -> f
		| ADD (exp1, exp2) -> (foo (exp1, boolX, valX)) +. (foo (exp2, boolX, valX))
		| SUB (exp1, exp2) -> (foo (exp1, boolX, valX)) -. (foo (exp2, boolX, valX))
		| MUL (exp1, exp2) -> (foo (exp1, boolX, valX)) *. (foo (exp2, boolX, valX))
		| DIV (exp1, exp2) ->
			let t = foo (exp2, boolX, valX) in
			if t = 0.0
				then raise DividedByZero
				else (foo (exp1, boolX, valX)) /. t
		| SIGMA (exp1, exp2, exp3) ->
			let lower_limit = int_of_float (foo (exp1, boolX, valX)) in
			let upper_limit = int_of_float (foo (exp2, boolX, valX)) in
			if lower_limit > upper_limit
				then 0.0
				else (foo (exp3, true, float_of_int lower_limit)) +. (foo ((SIGMA (INT (lower_limit+1), exp2, exp3), boolX, valX)))
		| INTEGRAL (exp1, exp2, exp3) ->
			let lower_limit = foo (exp1, boolX, valX) in
			let upper_limit = foo (exp2, boolX, valX) in
			if lower_limit > upper_limit
				then 0.0 -. (foo ((INTEGRAL (exp2, exp1, exp3)), boolX, valX))
				else
					if lower_limit +. 0.1 > upper_limit
						then (foo (exp3, true, lower_limit)) *. (upper_limit -. lower_limit)
					 	else ((foo (exp3, true, lower_limit)) *. 0.1)
			    				+. (foo ((INTEGRAL ((REAL (lower_limit +. 0.1)), exp2, exp3)), boolX, valX)) in 
	foo (exp, false, 0.0)
