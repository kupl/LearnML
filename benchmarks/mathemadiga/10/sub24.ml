(* Exercise 2 *)
exception FreevarError
exception DividedByZero

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

type var =
	NONE
	| VAL of float

let rec eval (exp, var) =
	match exp with
		X ->
			(match var with
				NONE ->
					raise FreevarError
				| VAL a ->
					a)
		| INT a ->
			(float_of_int a)
		| REAL a ->
			a
		| ADD (a, b) ->
			((eval (a, var)) +. (eval (b, var)))
		| SUB (a, b) ->
			((eval (a, var)) -. (eval (b, var)))
		| MUL (a, b) ->
			((eval (a, var)) *. (eval (b, var)))
		| DIV (a, b) ->
			let evalb = (eval (b, var)) in
			if (evalb = 0.) then
				raise DividedByZero
			else
				((eval (a, var)) /. evalb)
		| SIGMA (a, b, f) ->
			let evala = (eval (a, var)) in
			let evalb = (eval (b, var)) in
			if (evala > evalb) then
				0.
			else if (evala = evalb) then
				(eval (f, (VAL evala)))
			else
				let nowterm = (eval (f, (VAL evala))) in
				let nextterm = (eval ((SIGMA ((INT ((int_of_float evala) + 1)), b, f)), var)) in
				(nowterm +. nextterm)
		| INTEGRAL (a, b, f) ->
			let evala = (eval (a, var)) in
			let evalb = (eval (b, var)) in
			if (evala > evalb) then
				(~-. (eval ((INTEGRAL (b, a, f)), var)))
			else if ((evalb -. evala) < 0.1) then
				let width = (evalb -. evala) in
				let height = (eval (f, (VAL evala))) in
				(width *. height)
			else
				let nowterm = (0.1 *. (eval (f, (VAL evala)))) in
				let nextterm = (eval ((INTEGRAL ((REAL (evala +. 0.1)), b, f)), var)) in
				(nowterm +. nextterm)

let mathemadiga exp =
	eval (exp, NONE)
