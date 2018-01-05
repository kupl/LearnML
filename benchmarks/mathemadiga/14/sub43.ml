exception Todo (*done done*)
exception Stodo
exception Itodo
exception FreeVariable

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

let rec substitute (e, key: exp * float): float = 
	match e with
	| X -> key
	| INT i -> float_of_int i
	| REAL r -> r
	| ADD (e1, e2) -> substitute (e1, key) +. substitute (e2, key) 
	| SUB (e1, e2) -> substitute (e1, key) -. substitute (e2, key) 
	| MUL (e1, e2) -> substitute (e1, key) *. substitute (e2, key) 
	| DIV (e1, e2) -> substitute (e1, key) /. substitute (e2, key) 
	| SIGMA (e1, e2, f) -> sigma (int_of_float (substitute (e1,key)), int_of_float (substitute (e2,key)), f, 0.0)
	| INTEGRAL (e1, e2, f) -> integral (substitute (e1,key), substitute (e2,key), f, 0.0)

and sigma ((lbound, hbound, f, sum): (int * int * exp * float)): float =	
	if lbound > hbound then sum
	else if lbound = hbound then (substitute (f, float_of_int lbound)) +. sum
	else sigma (lbound + 1, hbound, f, (substitute (f, float_of_int lbound)) +. sum)
	
and integral ((lbound, hbound, f, sum): (float * float * exp * float)): float = 
	if lbound > hbound then -1.0 *. (integral (hbound, lbound, f, sum))
	else if hbound -. lbound < 0.1 then sum
	else integral (lbound +. 0.1, hbound, f, ((substitute (f, lbound)) *. 0.1) +. sum)
	
let rec galculator (e: exp): float = 
	match e with
	| X -> raise FreeVariable
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, f) -> sigma (int_of_float (galculator e1), int_of_float (galculator e2), f, 0.0)
	| INTEGRAL (e1, e2, f) -> integral (galculator e1, galculator e2, f, 0.0)
