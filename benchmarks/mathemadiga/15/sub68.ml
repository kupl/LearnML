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

let galculator expr =

let rec galculator' expr =
	let rec sigma (a, b, f) =
		let a' = int_of_float a and b' = int_of_float b in
		if a' > b' then 0.
		else (f (float_of_int a')) +. sigma (float_of_int (a' + 1), float_of_int b', f)
	in
	let rec integral (a, b, f) =
		if a > b then (-. integral (b, a, f))
		else if b < a +. 0.1 then 0.0
		else (f a) *. 0.1 +. integral (a +. 0.1, b, f)
	in
	match expr with
	| X -> fun x -> x
	| INT i -> fun x -> float_of_int i
	| REAL f -> fun x -> f
	| ADD (exp1, exp2) -> fun x -> (galculator' exp1 x +. galculator' exp2 x)
	| SUB (exp1, exp2) -> fun x -> (galculator' exp1 x -. galculator' exp2 x)
	| MUL (exp1, exp2) -> fun x -> (galculator' exp1 x *. galculator' exp2 x)
	| DIV (exp1, exp2) -> fun x -> (galculator' exp1 x /. galculator' exp2 x)
	| SIGMA (a, b, f) -> fun x -> sigma (galculator' a x, galculator' b x, galculator' f)
	| INTEGRAL (a, b, f) -> fun x -> integral (galculator' a x, galculator' b x, galculator' f)

in
	let res0 = galculator' expr 0.0 and res1 = galculator' expr 98.76 in
	if res0 = res1 then res0
	else raise(FreeVariable)

