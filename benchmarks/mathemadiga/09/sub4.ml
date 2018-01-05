(* Computer Science/2005-11759/Sangcheol Park/Exercise 2-5.*)

exception FreeVariable
exception InvalidSigma
exception DivideByZero

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
;;

let rec mathemadiga exp =
	let dx = 0.1 in
	let rec x_eval_real(x_formula, x_value) =
		match x_formula with
		| X -> x_value
		| INT a -> float_of_int a
		| REAL a -> a
		| ADD(a, b) -> x_eval_real (a, x_value) +. x_eval_real (b, x_value)
		| SUB(a, b) -> x_eval_real (a, x_value) -. x_eval_real (b, x_value)
		| MUL(a, b) -> x_eval_real (a, x_value) *. x_eval_real (b, x_value)
		| DIV(a, b) ->
				if (x_eval_real (b, x_value) != 0.0)
				then x_eval_real (a, x_value) /. x_eval_real (b, x_value)
				else raise DivideByZero
		| SIGMA(a, b, c) -> mathemadiga (SIGMA(a, b, c))
    | INTEGRAL(a, b, c) -> mathemadiga (INTEGRAL(a, b, c)) in
	let rec sigma_helper(a, b, c) = if (a <= b) then x_eval_real(c, a) +. sigma_helper(a +. 1.0, b, c) else 0.0 in
	let rec integral_helper(a, b, c, e) = if (a <= b) then x_eval_real(c, a) *. dx ** (1.0 +.e) +. integral_helper(a +. dx ** (1.0 +.e), b, c, e) else 0.0 in
	let rec num_decimal_digits(f) =
		if (f = floor(f)) then 0.0 else 1.0 +. num_decimal_digits(10.0 *. f) in
	let rec cut_decimal_digits(f, n) = floor ((10.0 ** n) *. f) /. (10.0 ** n) in
	let rec integral_helper2(a, b, c) =
		let a_dd = num_decimal_digits(a) in
		let b_dd = num_decimal_digits(b) in
		integral_helper(a, b, c, max a_dd b_dd) in
	match exp with
	| X -> raise FreeVariable
	| INT a -> float_of_int a
	| REAL a -> a
	| ADD(a, b) -> (mathemadiga a) +. (mathemadiga b)
	| SUB(a, b) -> (mathemadiga a) -. (mathemadiga b)
	| MUL(a, b) -> (mathemadiga a) *. (mathemadiga b)
	| DIV(a, b) ->
			if ((mathemadiga b) != 0.0)
			then (mathemadiga a) /. (mathemadiga b)
			else raise DivideByZero
	| INTEGRAL(a, b, c) ->
			let a_value = mathemadiga a in
			let b_value = mathemadiga b in
			if (a_value > b_value) then 0.0 -. integral_helper2(b_value, a_value, c)
			else integral_helper2(a_value, b_value, c)
	| SIGMA(a, b, c) ->
			let a_value = mathemadiga a in
			let b_value = mathemadiga b in
			if (a_value > b_value) then raise InvalidSigma
			else sigma_helper(a_value, b_value, c)
;;