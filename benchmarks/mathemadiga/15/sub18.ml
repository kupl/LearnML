(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #5 *)
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

let rec intEval (e, n) : float =
	match e with
	| X -> float_of_int n
	| INT x -> float_of_int x
	| REAL x -> x
	| ADD(a, b) -> intEval(a, n) +. intEval(b, n)
	| SUB(a, b) -> intEval(a, n) -. intEval(b, n)
	| MUL(a, b) -> intEval(a, n) *. intEval(b, n)
	| DIV(a, b) -> intEval(a, n) /. intEval(b, n)
	| SIGMA(a, b, c) -> galculator(SIGMA(REAL(intEval(a, n)), REAL(intEval(b, n)), c))
	| INTEGRAL(a, b, c) -> galculator(INTEGRAL(REAL(intEval(a, n)), REAL(intEval(b, n)), c))

and floatEval (e, f) : float =
	match e with
	| X -> f
	| INT x -> float_of_int x
	| REAL x -> x
	| ADD(a, b) -> floatEval(a, f) +. floatEval(b, f)
	| SUB(a, b) -> floatEval(a, f) -. floatEval(b, f)
	| MUL(a, b) -> floatEval(a, f) *. floatEval(b, f)
	| DIV(a, b) -> floatEval(a, f) /. floatEval(b, f)
	| SIGMA(a, b, c) -> galculator(SIGMA(REAL(floatEval(a, f)), REAL(floatEval(b, f)), c))
	| INTEGRAL(a, b, c) -> galculator(INTEGRAL(REAL(floatEval(a, f)), REAL(floatEval(b, f)), c))

and galculator (e : exp) : float =
	match e with
	| X -> raise FreeVariable
	| INT x -> float_of_int x
	| REAL x -> x
	| SIGMA(a, b, f) -> if ((int_of_float(galculator(a))) > (int_of_float(galculator(b)))) then 0.
						else let t = ref 0. in
							for x = (int_of_float(galculator(a))) to (int_of_float(galculator(b))) do
								t := !t +. intEval(f, x)
							done;
							!t
	| INTEGRAL(a, b, f) -> if (galculator(a) > galculator(b)) then -1. *. galculator (INTEGRAL(b, a, f))
							else if ((galculator(b) -. galculator(a)) < 0.1) then 0.
							else let t = ref 0. in let x = ref (galculator(a)) in
								while (!x +. 0.1) <= galculator(b) do
									t := !t +. (floatEval(f, !x) *. 0.1);
									x := !x +. 0.1
								done;
								!t
	| ADD(a, b) -> galculator(a) +. galculator(b)
	| SUB(a, b) -> galculator(a) -. galculator(b)
	| MUL(a, b) -> galculator(a) *. galculator(b)
	| DIV(a, b) -> galculator(a) /. galculator(b)
