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
	| INT x -> float_of_int x
	| REAL x -> x
	| ADD (exp1, exp2) -> (galculator exp1) +. (galculator exp2)
	| SUB (exp1, exp2) -> (galculator exp1) -. (galculator exp2)
	| MUL (exp1, exp2) -> (galculator exp1) *. (galculator exp2)
	| DIV (exp1, exp2) -> (galculator exp1) /. (galculator exp2)
	| SIGMA (exp1, exp2, exp3) -> 
		let val1 = (floor (galculator exp1)) in
		let val2 = (floor (galculator exp2)) in
		if val1 > val2 then 0.
		else my_cal (val1, val2, exp3, 1.0)
	| INTEGRAL (exp1, exp2, exp3) ->
		let val1 = (galculator exp1) in
		let val2 = (galculator exp2) in
		let gap = (val2 -. val1) in
		if (abs_float gap) < 0.1 then 0.
		else if gap > 0. then my_cal (val1, val2, exp3, 0.1)
		else -1. *. my_cal (val2, val1, exp3, 0.1)

and my_cal (val1, val2, exp, interval) = 
	let return = interval *. getval(val1, exp) in
	if val2 -. val1 < 0.1 then (floor interval) *. return
	else return +. my_cal ((val1 +. interval), val2, exp, interval)

and getval (arg1, exp) =
	match exp with
	| X -> arg1
	| INT x -> float_of_int x
	| REAL x -> x
	| ADD (exp1, exp2) -> (getval (arg1, exp1)) +. (getval (arg1, exp2))
	| SUB (exp1, exp2) -> (getval (arg1, exp1)) -. (getval (arg1, exp2))
	| MUL (exp1, exp2) -> (getval (arg1, exp1)) *. (getval (arg1, exp2))
	| DIV (exp1, exp2) -> (getval (arg1, exp1)) /. (getval (arg1, exp2))
	| SIGMA (exp1, exp2, exp3) -> 
		let val1 = (floor (getval (arg1, exp1))) in
		let val2 = (floor (getval (arg1, exp2))) in
		if val1 > val2 then 0.
		else my_cal (val1, val2, exp3, 1.0)
	| INTEGRAL (exp1, exp2, exp3) ->
		let val1 = (getval (arg1, exp1)) in
		let val2 = (getval (arg1, exp2)) in
		let gap = (val2 -. val1) in
		if abs_float gap < 0.1 then 0.
		else if gap > 0. then my_cal (val1, val2, exp3, 0.1)
		else -1. *. my_cal (val2, val1, exp3, 0.1)
