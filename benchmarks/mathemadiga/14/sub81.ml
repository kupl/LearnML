exception FreeVariable

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let rec eval_X expr x =
	match expr with
		| X -> x
		| INT i -> float_of_int i
		| REAL f -> f
		| ADD (e1, e2) -> (eval_X e1 x) +. (eval_X e2 x)
		| SUB (e1, e2) -> (eval_X e1 x) -. (eval_X e2 x)
		| MUL (e1, e2) -> (eval_X e1 x) *. (eval_X e2 x)
		| DIV (e1, e2) -> (eval_X e1 x) /. (eval_X e2 x)
		| SIGMA (e1, e2, e3) -> sigma (eval_X e1 x, eval_X e2 x, e3)
		| INTEGRAL (e1, e2, e3) -> integral (eval_X e1 x, eval_X e2 x, e3)
	and sigma (e1, e2, e3) =
		if (int_of_float e1) > (int_of_float e2) then 0.
		else if (int_of_float e1) = (int_of_float e2) then eval_X e3 (float_of_int (int_of_float e1))
		else (eval_X e3 (float_of_int (int_of_float e1))) +. sigma ((float_of_int (int_of_float e1)) +. 1., e2, e3)
	and integral (e1, e2, e3) =
		if e1 > e2 then integral (e2, e1, e3) *. -1.
		else if e2 -. e1 < 0.1 then 0.
		else 0.1 *. (eval_X e3 e1) +. integral (e1 +. 0.1, e2, e3) 

let rec galculator e =
	match e with
		| X -> raise (FreeVariable)
		| INT i -> float_of_int i
		| REAL f -> f
		| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
		| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
		| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
		| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
		| SIGMA (e1, e2, e3) -> sigma (galculator e1, galculator e2, e3)
		| INTEGRAL (e1, e2, e3) -> integral ((galculator e1), (galculator e2), e3)




