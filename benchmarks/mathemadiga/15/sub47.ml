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


let rec substitute_x : exp * exp -> exp = fun(e, f) -> 
	match f with
	| X -> e
	| INT _ -> f
	| REAL _ -> f
	| ADD(e1, e2) -> ADD(substitute_x (e, e1), substitute_x (e, e2))
	| SUB(e1, e2) -> SUB(substitute_x (e, e1), substitute_x (e, e2))
	| MUL(e1, e2) -> MUL(substitute_x (e, e1), substitute_x (e, e2))
	| DIV(e1, e2) -> DIV(substitute_x (e, e1), substitute_x (e, e2))
	| SIGMA(e1, e2, e3) -> 
		begin
			let newLB = substitute_x(e, e1) in
			let newUB = substitute_x(e, e2) in
			SIGMA (newLB, newUB, e3)
		end
	| INTEGRAL(e1, e2, e3) -> INTEGRAL(substitute_x (e, e1), substitute_x (e, e2), e3)


(* galculator: exp -> float *)
exception FreeVariable
let rec galculator e =
	match e with
	| X -> raise (FreeVariable)
	| INT i -> float_of_int i
	| REAL r -> r
	| ADD (e1, e2) -> galculator e1 +. galculator e2
	| SUB (e1, e2) -> galculator e1 -. galculator e2
	| MUL (e1, e2) -> galculator e1 *. galculator e2
	| DIV (e1, e2) -> galculator e1 /. galculator e2
	| SIGMA(e1, e2, e3) ->
		begin
			let i1 = int_of_float (galculator e1) in
			let i2 = int_of_float (galculator e2) in
  		if(i1 > i2) then 0.0
  		else if(i1 = i2) then galculator(substitute_x (INT i1, e3))
  		else
  			galculator(substitute_x(INT i1, e3)) +.
  			galculator(SIGMA ( INT(i1+1), INT i2, e3))
		end
	| INTEGRAL(e1, e2, e3) ->
		if (galculator e1 = galculator e2) then 0.0
		else if (galculator e1 > galculator e2) then
			galculator(INTEGRAL(e2, e1, e3)) *. (-1.0)
		else if (galculator e2 -. galculator e1 > 0.1) then
			galculator(substitute_x(e1, e3)) *. 0.1 +.
			galculator(INTEGRAL(REAL(galculator e1 +. 0.1), e2, e3))
		else 0.0		
		