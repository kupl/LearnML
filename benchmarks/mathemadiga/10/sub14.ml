type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreevarError
exception DividedByZero

let rec eval (a, exp, var) =
	match exp with
		X -> (if var=true then a else raise FreevarError)
		| INT i -> float_of_int i
		| REAL f -> f
		| ADD (e1, e2) -> (eval (a, e1, var)) +. (eval (a, e2, var))
		| SUB (e1, e2) -> (eval (a, e1, var)) -. (eval (a, e2, var))
		| MUL (e1, e2) -> (eval (a, e1, var)) *. (eval (a, e2, var))
		| DIV (e1, e2) -> (if (eval (a, e2, var)) = 0.0 then raise DividedByZero
								else (eval (a, e1, var)) /. (eval (a, e2, var)))
		| SIGMA (a1, b1, e) -> 
			(if (eval (a, a1, var) ) = (eval (a, b1, var)) then eval ((eval (a, a1, var)), e, true)
			 else (eval ((eval (a, a1, var)), e, true)) +. (eval (a, (SIGMA (REAL ((eval (a, a1, var)) +. 1.0), b1, e)), var))
			)
		| INTEGRAL (a1, b1, e) ->
			(if (eval (a, b1, var) ) < (eval (a, a1, var)) then 0.0-.(eval (a, INTEGRAL (b1, a1, e), var))
			 else
			 if (eval (a, b1, var) ) -. (eval (a, a1, var)) < 1.0 then (eval ((eval (a, a1, var)), e, true)) *.((eval (a, b1, var) ) -. (eval (a, a1, var))) 
			 else ((eval ((eval (a, a1, var)), e, true)) *. 0.1) +. (eval (a, (INTEGRAL (REAL ((eval (a, a1, var)) +. 0.1), b1, e)), var))
			);;

let mathemadiga exp =
	eval (0.0, exp, false);;
