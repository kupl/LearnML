exception FreeVariable
exception InvalidArgument

type exp =
	| X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let rec all_int_to_float exp =
	match exp with
	| INT n -> REAL (float n)
	| ADD (a, b) -> ADD (all_int_to_float a, all_int_to_float b)
	| SUB (a, b) -> SUB (all_int_to_float a, all_int_to_float b)
	| MUL (a, b) -> MUL (all_int_to_float a, all_int_to_float b)
	| DIV (a, b) -> DIV (all_int_to_float a, all_int_to_float b)
	| SIGMA (a, b, c) -> SIGMA (all_int_to_float a, all_int_to_float b, all_int_to_float c)
	| INTEGRAL (a, b, c) -> INTEGRAL (all_int_to_float a, all_int_to_float b, all_int_to_float c)
	| a -> a

let float_to_exp x = REAL x

let rec is_X_Exist exp =
	match exp with
	| X -> true
	| ADD (a, b) | SUB (a, b) | MUL (a, b) | DIV (a, b) ->
			(is_X_Exist a) || (is_X_Exist b)
	| SIGMA (a, b, c) | INTEGRAL (a, b, c) ->
			(is_X_Exist a) || (is_X_Exist b) || (is_X_Exist c)
	| _ -> false

let rec insert exp x =
	match exp with
	| X -> x
	| INT n -> float n
	| REAL r -> r
	| ADD (a, b) -> (insert a x) +. (insert b x)
	| SUB (a, b) -> (insert a x) -. (insert b x)
	| MUL (a, b) -> (insert a x) *. (insert b x)
	| DIV (a, b) -> (insert a x) /. (insert b x)
	| SIGMA (a, b, c) -> galculator (SIGMA (float_to_exp (insert a x), float_to_exp (insert b x), c))
	| INTEGRAL (a, b, c) -> galculator (INTEGRAL (float_to_exp (insert a x), float_to_exp (insert b x), c))

and galculator exp =
	match exp with
	| X -> raise FreeVariable
	| INT n -> float n
	| REAL r -> r
	| ADD (INT a, INT b) -> float a +. float b
	| ADD (REAL a, INT b) | ADD (INT b, REAL a) -> a +. float b
	| ADD (REAL a, REAL b) -> a +. b
	| ADD (a, b) -> galculator a +. galculator b
	| SUB (INT a, INT b) -> float a -. float b
	| SUB (REAL a, INT b) -> a -. float b
	| SUB (INT a, REAL b) -> float a -. b
	| SUB (REAL a, REAL b) -> a -. b
	| SUB (a, b) -> galculator a -. galculator b
	| MUL (INT a, INT b) -> float a *. float b
	| MUL (REAL a, INT b) | MUL (INT b, REAL a) -> a *. float b
	| MUL (REAL a, REAL b) -> a *. b
	| MUL (a, b) -> galculator a *. galculator b
	| DIV (INT a, INT b) -> float a /. float b
	| DIV (REAL a, INT b) -> a /. float b
	| DIV (INT a, REAL b) -> float a /. b
	| DIV (REAL a, REAL b) -> a /. b
	| DIV (a, b) -> galculator a /. galculator b
	| SIGMA (INT a, INT b, c) ->
			if a > b then 0.
			else (insert (all_int_to_float c) (float a)) +. galculator (SIGMA (INT (a +1), INT b, c))
	| SIGMA (REAL a, b, c) -> galculator (SIGMA (INT (int_of_float a), b, c))
	| SIGMA (a, REAL b, c) -> galculator (SIGMA (a, INT (int_of_float b), c))
	| SIGMA (a, b, c) ->
			(*if (is_X_Exist a || is_X_Exist b) then raise FreeVariable
			else*) galculator (SIGMA (float_to_exp (galculator a), float_to_exp (galculator b), c))
	| INTEGRAL (REAL a, REAL b, c) ->
		  if abs_float (a -. b) < 0.1 then 0.
			else if a > b then (galculator (INTEGRAL (REAL b, REAL a, c)) *. -1.)
			else (0.1 *. (insert (all_int_to_float c) a)) +. galculator (INTEGRAL (REAL (a +. 0.1), REAL b, c))
	| INTEGRAL (INT a, b, c) -> galculator (INTEGRAL (REAL (float a), b, c))
	| INTEGRAL (a, INT b, c) -> galculator (INTEGRAL (a, REAL (float b), c))
	| INTEGRAL (a, b, c) ->
			(*if (is_X_Exist a || is_X_Exist b) then raise FreeVariable
			else*) galculator (INTEGRAL (float_to_exp (galculator a), float_to_exp (galculator b), c))