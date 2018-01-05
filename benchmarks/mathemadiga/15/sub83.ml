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

let rec calc (e: exp) (x: float): float =
	match e with
	| X -> x
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> (calc e1 x) +. (calc e2 x)
	| SUB (e1, e2) -> (calc e1 x) -. (calc e2 x)
	| MUL (e1, e2) -> (calc e1 x) *. (calc e2 x)
	| DIV (e1, e2) -> (calc e1 x) /. (calc e2 x)
	| _ -> 0.
	
let rec cSig (f1: float) (f2: float) (e: exp): float = 
	if f1 <= f2 then (calc e f1) +. (cSig (f1 +. 1.0) f2 e) else 0. 

let rec cInt f1 f2 e: float =
	if f1 > f2 then 0. -. (cInt f2 f1 e)
	else if f1 <= ( f2 -. 0.1 ) then ((calc e f1) *. 0.1) +. (cInt (f1 +. 0.1) f2 e)
	else 0.
	
let rec galculator (e: exp): float = 
	match e with
	| X -> raise FreeVariable
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, e3) -> cSig (galculator e1) (galculator e2) e3
	| INTEGRAL (e1, e2, e3) -> cInt (galculator e1) (galculator e2) e3
	
	

	