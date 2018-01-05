type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception InvalidSigma
exception FreeVariable

let mathemadiga exp =
	let rec mathmodule expr x fv=
		match expr with
		| X -> 
			if fv then (mathmodule x (INT 0) true)
			else raise FreeVariable
		| INT i -> (float)i
		| REAL r -> r
		| ADD (exp1, exp2) -> (mathmodule exp1 x fv) +. (mathmodule exp2 x fv)
		| SUB (exp1, exp2) -> (mathmodule exp1 x fv) -. (mathmodule exp2 x fv)
		| MUL (exp1, exp2) -> (mathmodule exp1 x fv) *. (mathmodule exp2 x fv)
		| DIV (exp1, exp2) -> (mathmodule exp1 x fv) /. (mathmodule exp2 x fv)
		| SIGMA (exp1, exp2, exp3) ->
			(match (exp1, exp2) with
			| (INT a, INT b) -> 
				if b - a >= 0 then (mathmodule exp3 exp1 true) +. (mathmodule (SIGMA ((INT (a+1)), exp2, exp3)) x true)
				else 0.0
			| (REAL a, _) -> mathmodule (SIGMA (INT (int_of_float a), exp2, exp3)) x fv
			| (_, REAL b) -> mathmodule (SIGMA (exp1, INT (int_of_float b), exp3)) x fv
			| _ -> raise InvalidSigma
			)
		| INTEGRAL (exp1, exp2, exp3) -> 
			(let b = (mathmodule exp2 x fv) in
				let a = (mathmodule exp1 x fv) in
					let diff = b -. a in
						if diff < 0.0  then 0.0 -. (mathmodule (INTEGRAL ((REAL b), (REAL a), exp3)) x true)
						else if diff <= 0.1 then (mathmodule exp3 (REAL a) true) *. diff
						else (mathmodule exp3 (REAL a) true) *. 0.1 +. (mathmodule (INTEGRAL ((REAL (a +. 0.1)), (REAL b), exp3)) x true)
			)

		in

	mathmodule exp (INT 0) false
	;;
