(* mathemadiga: exp -> float *)
exception FreeVariable
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

let rec sigma a b f =
	if a > b then 0.0
	else if a = b then (f (float_of_int a))
	else (f (float_of_int a)) +. (sigma (a+1) b f)

let rec integral a b f =
	let rec integ a b f =
		if a >= b then 0.0
		else ((f a) *. 0.1) +. (integ (a+.0.1) b f) in
	if a > b then -. (integ b a f)
	else (integ a b f)

let rec assign e x = match e with
X -> x
| INT i -> float_of_int i
| REAL f -> f
| ADD (e0, e1) -> (assign e0 x) +. (assign e1 x)
| SUB (e0, e1) -> (assign e0 x) -. (assign e1 x)
| MUL (e0, e1) -> (assign e0 x) *. (assign e1 x)
| DIV (e0, e1) -> if (assign e1 x) = 0.0 then raise DivideByZero
					 else (assign e0 x) /. (assign e1 x)
| SIGMA (b, t, exp) -> sigma (int_of_float (assign b x))
			     (int_of_float (assign t x))
			     (assign exp)
| INTEGRAL (b, t, exp) -> integral (assign b x)
				   (assign t x)
				   (assign exp)

let rec mathemadiga e = match e with
X -> raise FreeVariable
| INT i -> float_of_int i
| REAL f -> f
| ADD (e0, e1) -> (mathemadiga e0) +. (mathemadiga e1)
| SUB (e0, e1) -> (mathemadiga e0) -. (mathemadiga e1)
| MUL (e0, e1) -> (mathemadiga e0) *. (mathemadiga e1)
| DIV (e0, e1) -> if (mathemadiga e1) = 0.0 then raise DivideByZero
					    else (mathemadiga e0) /. (mathemadiga e1)
| SIGMA (b, t, exp) -> sigma (int_of_float (mathemadiga b))
			     (int_of_float (mathemadiga t))
			     (assign exp)
| INTEGRAL (b, t, exp) -> integral (mathemadiga b)
				   (mathemadiga t)
			           (assign exp)

