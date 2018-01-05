type exp = 
X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreeVariable

type environment = EMPTY | ENV of float

let rec galc e env = 
	match e with
	| X -> 	(match env with
			| EMPTY -> raise FreeVariable
			| ENV f -> f)
	| INT n -> float_of_int n
	| REAL r -> r
	| ADD(e1,e2) -> (galc e1 env) +. (galc e2 env)
	| SUB(e1,e2) -> (galc e1 env) -. (galc e2 env)
	| MUL(e1,e2) -> (galc e1 env) *. (galc e2 env)
	| DIV(e1,e2) -> (galc e1 env) /. (galc e2 env)
	| SIGMA(e1,e2,e3) -> 
		let ge1 = int_of_float (galc e1 env) in
		let ge2 = int_of_float (galc e2 env) in
		if ge1 > ge2 then 0.
		else
			let newEnv = ENV (float_of_int ge1) in
			(galc e3 newEnv) +. (galc (SIGMA(INT(ge1 + 1), e2, e3)) env)
	| INTEGRAL(e1,e2,e3) ->
		let ge1 = galc e1 env in
		let ge2 = galc e2 env in
		if ge1 +. 0.1 > ge2 then 0.
		else 
			let newEnv = ENV ge1 in
			((galc e3 newEnv) *. 0.1) +. (galc (INTEGRAL(REAL(ge1 +. 0.1), e2, e3)) env)
		
let galculator e = 
	(galc e EMPTY)