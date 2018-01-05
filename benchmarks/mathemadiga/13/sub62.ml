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

let rec galculator e = 
	match e with
	| X -> raise FreeVariable
	| INT ex -> float_of_int ex
	| REAL ex -> ex
	| ADD (ex1, ex2) -> (galculator ex1) +. (galculator ex2)
	| SUB (ex1, ex2) -> (galculator ex1) -. (galculator ex2)
	| MUL (ex1, ex2) -> (galculator ex1) *. (galculator ex2)
	| DIV (ex1, ex2) -> (galculator ex1) /. (galculator ex2)
	| SIGMA (ex1, ex2, ex3) -> sigma(int_of_float(galculator ex1), int_of_float(galculator ex2), ex3) 
	| INTEGRAL (ex1, ex2, ex3) ->  integral((galculator ex1), (galculator ex2), ex3)
and f(e,x) =
	match e with
	X -> x
        | INT ex -> float_of_int ex
        | REAL ex -> ex
        | ADD (ex1, ex2) -> f(ex1,x) +. f(ex2,x) 
        | SUB (ex1, ex2) -> f(ex1,x) -. f(ex2,x) 
        | MUL (ex1, ex2) -> f(ex1,x) *. f(ex2,x) 
        | DIV (ex1, ex2) -> f(ex1,x) /. f(ex2,x) 
        | SIGMA (ex1, ex2, ex3) -> sigma(int_of_float(f(ex1,x)), int_of_float(f(ex2,x)), ex3)
        | INTEGRAL (ex1, ex2, ex3) -> integral((f(ex1,x)), (f(ex2,x)), ex3)

and sigma(a,b,e) =
	if a > b then 0.0 else f(e, float_of_int a) +. sigma(a+1, b, e)

and integral(a,b,e) =
	if a > b then (-1.0 *. integral(b,a,e) )
	else if (b -. a) < 0.1 then 0.0
	else 0.1*.(f(e,a)) +. integral(a +. 0.1, b, e)
	
