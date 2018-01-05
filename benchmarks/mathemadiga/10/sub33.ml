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
exception Error of string
(*
let rec to_str exp =
	match exp with
	  X -> "X"
	| INT i -> string_of_int i
	| REAL f -> string_of_float f
	| ADD (e1, e2) -> (to_str e1)^" + "^(to_str e2)
	| SUB (e1, e2) -> (to_str e1)^" - "^(to_str e2)
	| MUL (e1, e2) -> (to_str e1)^" * "^(to_str e2)
	| DIV (e1, e2) -> (to_str e1)^" / "^(to_str e2)
	| SIGMA (e1, e2, e3) -> "sigma ("^(to_str e1)^", "^(to_str e2)^", "^(to_str e3)^")"
	| INTEGRAL (e1, e2, e3) -> "integral ("^(to_str e1)^", "^(to_str e2)^", "^(to_str e3)^")"
*)
let rec mathemadiga exp =
	match exp with
	  X -> raise FreevarError
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> (mathemadiga e1) +. (mathemadiga e2)
	| SUB (e1, e2) -> (mathemadiga e1) -. (mathemadiga e2)
	| MUL (e1, e2) -> (mathemadiga e1) *. (mathemadiga e2)
	| DIV (e1, e2) -> (mathemadiga e1) /. (let temp = mathemadiga e2 in if temp = 0. then raise DividedByZero else temp)
	| SIGMA (INT val1, INT val2, e3) ->
		if val1 = val2 then evalx e3 (float_of_int val1)
		else if val1 > val2 then raise (Error "Invalid range")
		else (evalx e3 (float_of_int val1)) +. (mathemadiga (SIGMA(INT (val1 + 1), INT val2, e3)))
	| SIGMA _ -> raise (Error "liar! in mathemadiga")
	| INTEGRAL (e1, e2, e3) -> 
		let val1 = mathemadiga e1 in
		let val2 = mathemadiga e2 in
		if val1 = val2 then 0.0
		else if val1 > val2 then (-1.0) *. (mathemadiga (INTEGRAL(REAL val2, REAL val1, e3)))
		else if (val2 -. val1) < 0.1 then (evalx e3 val1) *. (val2 -. val1)
		else ((evalx e3 val1) *. 0.1) +. (mathemadiga (INTEGRAL(REAL (val1 +. 0.1), REAL val2, e3)))
and evalx exp x =
	match exp with
	  X -> x
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> (evalx e1 x) +. (evalx e2 x)
	| SUB (e1, e2) -> (evalx e1 x) -. (evalx e2 x)
	| MUL (e1, e2) -> (evalx e1 x) *. (evalx e2 x)
	| DIV (e1, e2) -> (evalx e1 x) /. (evalx e2 x)
	| SIGMA (INT _, INT _, _) -> mathemadiga exp
	| SIGMA _ -> raise (Error "liar! in evalx")
	| INTEGRAL (e1, e2, e3) ->
		let val1 = evalx e1 x in
		let val2 = evalx e2 x in
		mathemadiga (INTEGRAL (REAL val1, REAL val2, e3))
