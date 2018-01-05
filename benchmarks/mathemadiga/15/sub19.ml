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

let rec galculator exp =
	let rec cal exp v =
		match exp with
		| X -> v
		| INT a -> float_of_int(a)
		| REAL a -> a
		| ADD (a,b) -> (cal a v)+.(cal b v)
		| SUB (a,b) -> (cal a v)-.(cal b v)
		| MUL (a,b) -> (cal a v)*.(cal b v)
		| DIV (a,b) -> (cal a v)/.(cal b v)
		| SIGMA (a,b,c) -> 
			if (galculator a)>(galculator b) then 0.
			else (cal c (galculator a))+.(cal (SIGMA(INT(int_of_float((galculator a)+.1.)),b,c)) 0.)
		| INTEGRAL (a,b,c) -> 
			if (galculator a)>(galculator b) then -1.*.(cal (INTEGRAL (b,a,c)) v)
			else if ((galculator b)-.(galculator a))<0.1 then 0.
			else (cal c (galculator a))*.0.1 +.(cal (INTEGRAL( REAL((galculator a)+.0.1),b,c)) 0.)
	in
	match exp with
	| X -> raise FreeVariable
	| INT a -> float_of_int(a)
	| REAL a -> a
	| ADD (a,b) -> (galculator a)+.(galculator b)
	| SUB (a,b) -> (galculator a)-.(galculator b)
	| MUL (a,b) -> (galculator a)*.(galculator b)
	| DIV (a,b) -> (galculator a)/.(galculator b)
	| SIGMA (a,b,c) -> cal (SIGMA (a,b,c)) 0.
	| INTEGRAL (a,b,c) -> cal (INTEGRAL (a,b,c)) 0.


