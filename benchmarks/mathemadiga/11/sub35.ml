exception Error

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
	
let mathemadiga e =
	let rec entrance ex num bs =
		match ex with
			| X -> if bs then num else raise Error
			| INT(n) -> float_of_int n
			| REAL(r) -> r
			| ADD(a,b) -> (entrance a num bs) +. (entrance b num bs)
			| SUB(a,b) -> (entrance a num bs) -. (entrance b num bs)
			| MUL(a,b) -> (entrance a num bs) *. (entrance b num bs)
			| DIV(a,b) -> (entrance a num bs) /. (entrance b num bs)
			| SIGMA(INT(a),INT(b),c) ->
				if a>b then 0.0
				else (entrance c (float_of_int a) true) +. (entrance (SIGMA(INT(a+1),INT(b),c)) num bs)
			| SIGMA(REAL(a),INT(b),c) ->
				entrance (SIGMA(INT(int_of_float a),INT(b),c)) num bs
			| SIGMA(INT(a),REAL(b),c) ->
				entrance (SIGMA(INT(a),INT(int_of_float b),c)) num bs
			| SIGMA(REAL(a),REAL(b),c) ->
				entrance (SIGMA(INT(int_of_float a),INT(int_of_float b),c)) num bs
			| SIGMA(_,_,_) -> raise Error
			| INTEGRAL(REAL(a),REAL(b),c) -> 
				if a > b then -.(entrance (INTEGRAL(REAL(b),REAL(a),c)) num bs)
				else if (a+.0.1) >= b then (b -. a) *. (entrance c a true)
				else 0.1 *. (entrance c a true) +. (entrance (INTEGRAL(REAL(a+.0.1),REAL(b),c)) num bs)
			| INTEGRAL(a,b,c) -> (entrance (INTEGRAL((REAL(entrance a num bs)),(REAL(entrance b num bs)),c)) num bs)
		in
	entrance e 0.0 false
