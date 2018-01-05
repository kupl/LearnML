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

let rec mathemadiga e =
	let rec calc (y, e) = match e with
		X -> y
	  | INT n -> float_of_int n
	  | REAL r -> r
	  | ADD(e1, e2) -> calc(y, e1) +. calc(y, e2)
	  | SUB(e1, e2) -> calc(y, e1) -. calc(y, e2)
	  | MUL(e1, e2) -> calc(y, e1) *. calc(y, e2)
	  | DIV(e1, e2) -> calc(y, e1) /. calc(y, e2)
	  | SIGMA(e1, e2, e3) -> mathemadiga(SIGMA(REAL(calc(y, e1)), REAL(calc(y, e2)), e3))
	  | INTEGRAL(e1, e2, e3) -> mathemadiga(INTEGRAL(REAL(calc(y, e1)), REAL(calc(y, e2)), e3))
	in
	match e with
		INT n -> float_of_int n
	  | REAL r -> r
	  | ADD(e1, e2) -> (mathemadiga e1) +. (mathemadiga e2)
	  | SUB(e1, e2) -> (mathemadiga e1) -. (mathemadiga e2)
	  | MUL(e1, e2) -> (mathemadiga e1) *. (mathemadiga e2)
	  | DIV(e1, e2) ->
			if (mathemadiga e2) = 0.0 then raise DividedByZero
			else (mathemadiga e1) /. (mathemadiga e2)
	  | SIGMA(INT i, INT j, e3) ->
			if i > j then 0.0
			else calc(float_of_int i, e3) +. mathemadiga(SIGMA(INT(i + 1), INT j, e3))
	  | INTEGRAL(e1, e2, e3) ->
			let a = mathemadiga e1 in
			let b = mathemadiga e2 in
			if a >= b then 0.0 (*-. mathemadiga(INTEGRAL(REAL b, REAL a, e3))*)
			else let d = if b -. a > 0.1 then 0.1 else b -. a in
				calc(a, e3) *. d +. mathemadiga(INTEGRAL(REAL(a +. 0.1), REAL b, e3))
	  | _ -> raise FreevarError