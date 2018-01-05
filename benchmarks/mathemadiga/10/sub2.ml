(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#2-2 *)

exception FreevarError
exception DividedByZero

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

type free = NAN
	| R of float

let dx = 0.1

let free_float x = match x with NAN -> raise FreevarError
	| R t -> t

let rec mathemadiga_wrap (e, x) = match e with X -> free_float x
			| INT v -> float_of_int v
			| REAL v -> v
			| ADD (e1, e2) -> mathemadiga_wrap (e1, x) +. mathemadiga_wrap (e2, x)
			| SUB (e1, e2) -> mathemadiga_wrap (e1, x) -. mathemadiga_wrap (e2, x)
			| MUL (e1, e2) -> mathemadiga_wrap (e1, x) *. mathemadiga_wrap (e2, x)
			| DIV (e1, e2) -> mathemadiga_wrap (e1, x) /. mathemadiga_wrap (e2, x)
			| SIGMA (INT a, INT b, exp) -> if (a > b) then 0.0
				else mathemadiga_wrap (SIGMA (INT (a + 1), INT b, exp), x) +. mathemadiga_wrap (exp, R (float_of_int a))
			| SIGMA (_, _, exp) -> raise FreevarError
			| INTEGRAL (e1, e2, exp) ->
				let diff = mathemadiga_wrap (e2, x) -. mathemadiga_wrap (e1, x) in
					if (diff < 0.0) then -. mathemadiga_wrap (INTEGRAL (e2, e1, exp), x)
					else if (diff < dx) then diff *. mathemadiga_wrap (exp, R (mathemadiga_wrap (e1, x)))
					else mathemadiga_wrap (INTEGRAL (REAL (mathemadiga_wrap (e1, x) +. dx), e2, exp), x) +. dx *. mathemadiga_wrap (exp, R (mathemadiga_wrap (e1, x)))

let mathemadiga e = mathemadiga_wrap (e, NAN)
(*
(* test code *)

let sigma1 = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))
let integral1 = INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1))
*)
