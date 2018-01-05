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

type xval = INVALID
	| VALINT of int
	| VALFLOAT of float
	
let rec calc: exp * xval -> exp =
	function (e, x) ->
	match e with
	| X -> (
		match x with
		| INVALID -> raise (FreeVariable)
		| VALINT a -> INT a
		| VALFLOAT a -> REAL a)
	| INT a -> INT a
	| REAL a -> REAL a
	| ADD(a, b) -> (
		let m = calc(a,x) in
		let n = calc(b,x) in
			match (m, n) with
			| (INT aa, INT bb) -> INT (aa + bb)
			| (INT aa, REAL bb) -> REAL ((float_of_int aa) +. bb)
			| (REAL aa, INT bb) -> REAL (aa +. (float_of_int bb))
			| (REAL aa, REAL bb) -> REAL (aa +. bb)
			| (expa, expb) -> INT 0)
	| SUB(a, b) -> (
		let m = calc(a,x) in
		let n = calc(b,x) in
			match (m, n) with
			| (INT aa, INT bb) -> INT (aa - bb)
			| (INT aa, REAL bb) -> REAL ((float_of_int aa) -. bb)
			| (REAL aa, INT bb) -> REAL (aa -. (float_of_int bb))
			| (REAL aa, REAL bb) -> REAL (aa -. bb)
			| (expa, expb) -> INT 0)
	| MUL(a, b) -> (
		let m = calc(a,x) in
		let n = calc(b,x) in
			match (m, n) with
			| (INT aa, INT bb) -> INT (aa * bb)
			| (INT aa, REAL bb) -> REAL ((float_of_int aa) *. bb)
			| (REAL aa, INT bb) -> REAL (aa *. (float_of_int bb))
			| (REAL aa, REAL bb) -> REAL (aa *. bb)
			| (expa, expb) -> INT 0)
	| DIV(a, b) -> (
		let m = calc(a,x) in
		let n = calc(b,x) in
			match (m, n) with
			| (INT aa, INT bb) -> INT (aa / bb)
			| (INT aa, REAL bb) -> REAL ((float_of_int aa) /. bb)
			| (REAL aa, INT bb) -> REAL (aa /. (float_of_int bb))
			| (REAL aa, REAL bb) -> REAL (aa /. bb)
			| (expa, expb) -> INT 0)
	| SIGMA(a, b, c) -> (
		let start = calc(a, x) in
		let start = (match start with INT n -> n | REAL n -> int_of_float n | start_ -> -1) in
			let finish = calc(b, x) in
			let finish = (match finish with INT n -> n | REAL n -> int_of_float n | finish_ -> -1) in
				if start > finish
				then INT 0
				else calc(ADD(c,SIGMA(INT(start+1),INT(finish),c)),VALINT start))
	| INTEGRAL(a, b, c) -> (
		let start = calc(a, x) in
		let start = (match start with INT n -> float_of_int n | REAL n -> n | start_ -> -1.) in
			let finish = calc(b, x) in
			let finish = (match finish with INT n -> float_of_int n | REAL n -> n | finish_ -> -1.) in
				if start > finish then calc(MUL(INTEGRAL(REAL finish, REAL start, c), REAL (-1.)),x) else
					if start +. 0.1 > finish then REAL 0.
					else calc(ADD(MUL(c, REAL 0.1), INTEGRAL(REAL (start+.0.1),REAL finish,c)), VALFLOAT start))
	
let galculator: exp -> float =
	function e ->
	let result = calc(e, INVALID) in
		match result with
		| INT n -> float_of_int n
		| REAL n -> n
		| els -> 0.