(* 200511843 LEE JONGHO *)


type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception InvalidSigma
exception FreeVariable

let rec mathenatica ex =  
	let rec f (i, e) =
		match e with	
		X -> float_of_int i
		| INT x -> float_of_int x
		| REAL x -> x 
		| ADD (e1, e2) -> f (i, e1) +. f (i, e2)
		| SUB (e1, e2) -> f (i, e1) -. f (i, e2)
		| MUL (e1, e2) -> f (i, e1) *. f (i, e2)
		| DIV (e1, e2) -> f (i, e1) /. f (i, e2)
		| SIGMA (e1, e2, e3) -> sigma (e1, e2, e3)
		| INTEGRAL (e1, e2, e3) -> integral (e1, e2, e3)

	and sigma (e1, e2, e3) =
		match (e1, e2) with
		(INT x, INT y) -> if x = y then f (x, e3)
					   else if x < y then f (x, e3) +. sigma (INT (x+1), e2, e3)
					   else raise InvalidSigma 
		| (_, _) -> raise InvalidSigma

	and g (i, e) =
		match e with
		X -> i
		| INT x -> float_of_int x
		| REAL x -> x 
		| ADD (e1, e2) -> g (i, e1) +. g (i, e2)
		| SUB (e1, e2) -> g (i, e1) -. g (i, e2)
		| MUL (e1, e2) -> g (i, e1) *. g (i, e2)
		| DIV (e1, e2) -> g (i, e1) /. g (i, e2)
		| SIGMA (e1, e2, e3) -> sigma (e1, e2, e3)
		| INTEGRAL (e1, e2, e3) -> integral (e1, e2, e3)		 
	
	and integral (e1, e2, e3) =
		match (e1, e2) with
		(REAL x, REAL y) -> if x = y then 0.0
					     else if x > y then (-1.0) *. integral (e2, e1, e3)
					     else if (y -. x) <= 0.1 then g (x, e3) *. (x -. y)
					     else (g (x, e3) *. 0.1)  +. integral (REAL (x +. 0.1), e2, e3)
		| (INT x, INT y) -> integral (REAL (float_of_int x), REAL (float_of_int y), e3)
		| (_, _) -> integral (REAL (mathenatica e1), REAL (mathenatica e2), e3) in

	  
	match ex with
	X -> raise FreeVariable
	| INT x -> float_of_int x
	| REAL x -> x
	| ADD (e1, e2) -> (mathenatica e1)+.(mathenatica e2)
	| SUB (e1, e2) -> (mathenatica e1)-.(mathenatica e2)
	| MUL (e1, e2) -> (mathenatica e1)*.(mathenatica e2)
	| DIV (e1, e2) -> (mathenatica e1)/.(mathenatica e2)
	| SIGMA (e1, e2, e3) -> sigma (e1, e2, e3)
	| INTEGRAL (e1, e2, e3) -> integral (e1, e2, e3)