(*물리천문학부 2011-11004 남윤석 문제 4*)

exception FreeVariable
type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA	of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let rec giVal(k, x) = match k with
	X -> x
	| INT v -> INT v
	| REAL v -> REAL v
	| ADD(v1, v2) -> ADD(giVal(v1, x), giVal(v2,x))
	| SUB(v1, v2) -> SUB(giVal(v1, x), giVal(v2,x))
	| MUL(v1, v2) -> MUL(giVal(v1, x), giVal(v2,x))
 	| DIV(v1, v2) -> DIV(giVal(v1, x), giVal(v2,x))
	| SIGMA(v1, v2, v3) -> SIGMA(v1, v2, v3)
	| INTEGRAL(v1, v2, v3) -> INTEGRAL(v1, v2, v3)


let rec galculator k = match k with 
	X -> raise FreeVariable 
	| INT v -> float_of_int(v)
	| REAL v -> v
	| ADD(v1, v2) -> galculator v1 +. galculator v2	
	| SUB(v1, v2) -> galculator v1 -. galculator v2
	| MUL(v1, v2) -> galculator v1 *. galculator v2
	| DIV(v1, v2) -> galculator v1 /. galculator v2
	| SIGMA(v1, v2, v3) ->
		let i1 = int_of_float(galculator v1) in
		let i2 = int_of_float(galculator v2) in
		if i1 > i2 then 0.0
		else if i1 < i2 then 
			galculator(giVal(v3, v1)) +. galculator(SIGMA(INT(i1 + 1), v2, v3))
		else galculator(giVal(v3, v1))	
	| INTEGRAL(v1, v2, v3) ->
		let d1 = galculator v1 in
		let d2 = galculator v2 in
		if (d1 -. d2 < 0.1) && (d2 -. d1 < 0.1) then 0.0
		else
			if d1 > d2 then
				-.galculator(INTEGRAL(v2, v1, v3))
			else if d1 < d2 then
				0.1 *. galculator(giVal(v3, v1)) +. galculator(INTEGRAL(REAL(d1 +. 0.1), v2, v3))
			else 0.1 *. galculator(giVal(v3, v1))
			


