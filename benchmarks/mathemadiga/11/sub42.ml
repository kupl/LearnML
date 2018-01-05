(*컴퓨터공학부 2009-11833 창배성*)
type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

(* X를 어떻게 처리해야 하는가*)

let rec mathemadiga x =
	match x with
	X -> X
	| ADD (INT a, INT b) -> INT (a + b)
	| ADD (REAL a, REAL b) -> REAL (a +. b)
	| SUB (INT a, INT b) -> INT (a - b)
	| SUB (REAL a, REAL b) -> REAL (a -. b)
	| MUL (INT a, INT b) -> INT (a * b)
	| MUL (REAL a, REAL b) -> REAL (a *. b)
	| DIV (INT a, INT b) -> INT (a / b)
	| DIV (REAL a, REAL b) -> REAL (a /. b)
	| ADD (a, b) -> ADD (mathemadiga a, mathemadiga b)
	| SUB (a, b) -> SUB (mathemadiga a, mathemadiga b)
	| MUL (a, b) -> MUL (mathemadiga a, mathemadiga b)
	| DIV (a, b) -> DIV (mathemadiga a, mathemadiga b)
	| INT a -> a
	| REAL b -> b
	| SIGMA (a, b, c) -> 1 (*
		if mathemadiga a > mathemadiga b then raise ("invalid input")
		else if mathemadiga a = mathemadiga b then (let X = a in mathemadiga c)
		else (X = a in mathemadiga c) +. mathemadiga (SIGMA (ADD (a, INT 1), b, c)) *)
	| INTEGRAL (a, b, c) -> 1 (*
		if mathemadiga a < mathemadiga b then (X = a in mathemadiga c) +. mathemadiga (INTEGRAL ( ADD (a, REAL 0.1), b, c))
		else if mathemadiga a = mathemadiga b then let X = a in mathemadiga c
		else raise ("invalid input") *)
	| _ -> raise ("Error")