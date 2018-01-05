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
			if (cal a v)>(cal b v) then 0.
			else (cal c (cal a v))+.(galculator (SIGMA(INT(int_of_float((cal a v)+.1.)), INT(int_of_float(cal b v)),c)))
		| INTEGRAL (a,b,c) -> 
			if (cal a v)>(cal b v) then -1.*.(cal (INTEGRAL (b,a,c)) v)
			else if ((cal b v)-.(cal a v))<0.1 then 0.
			else (cal c (cal a v))*.0.1 +.(galculator (INTEGRAL( REAL((cal a v)+.0.1),REAL(cal b v),c)) )
	in
	match exp with
	| X -> raise FreeVariable
	| INT a -> float_of_int(a)
	| REAL a -> a
	| ADD (a,b) -> (galculator a)+.(galculator b)
	| SUB (a,b) -> (galculator a)-.(galculator b)
	| MUL (a,b) -> (galculator a)*.(galculator b)
	| DIV (a,b) -> (galculator a)/.(galculator b)
	| SIGMA (a,b,c) -> cal (SIGMA (a,b,c)) (galculator a)
	| INTEGRAL (a,b,c) -> cal (INTEGRAL (a,b,c)) (galculator a)


