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

let rec equation = fun (a, f) -> 
	match f with
	|X -> a
	|INT x -> float_of_int x
	|REAL x -> x
	|ADD (x, y) -> (equation (a, x)) +. (equation (a, y)) 
	|SUB (x, y) -> (equation (a, x)) -. (equation (a, y))
	|MUL (x, y) -> (equation (a, x)) *. (equation (a, y))
	|DIV (x, y) -> (equation (a, x)) /. (equation (a, y))
	|SIGMA (x, y, f) -> sigma(float_of_int (int_of_float (equation (a, x))), float_of_int(int_of_float (equation (a, y))), f)
	|INTEGRAL (x, y, f) ->
		(if x>y then 0.0 -.(integral (equation (a, y), equation (a, x), f))
		else integral (equation (a, x), equation (a, y), f))

and sigma = fun (a, b, f) ->
	if a>b then (float_of_int 0)
	else equation (a, f) +.sigma(a+.1.0, b, f)

and integral = fun (a,b,f)->
	if b-.a<0.1 then (float_of_int 0)
	else (equation (a, f) *.0.1) +.integral (a+.0.1, b, f)

let rec galculator e = match e with
	X -> raise (FreeVariable)
	|INT x -> float_of_int x
	|REAL x -> x
	|ADD (x, y) -> (galculator x) +. (galculator y) 
	|SUB (x, y) -> (galculator x) -. (galculator y)
	|MUL (x, y) -> (galculator x) *. (galculator y)
	|DIV (x, y) -> (galculator x) /. (galculator y)
	|SIGMA (a, b, f) -> sigma(float_of_int (int_of_float (galculator a)), float_of_int(int_of_float (galculator b)), f)
	|INTEGRAL (a, b, f) ->
		(if a>b then 0.0 -.(integral (galculator b, galculator a, f))
		else integral (galculator a, galculator b, f))


