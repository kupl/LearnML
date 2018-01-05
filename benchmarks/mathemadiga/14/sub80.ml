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


let rec galculator : exp -> float = fun expr -> 
match expr with
	| X -> raise FreeVariable
	| INT n -> float_of_int n
	| REAL f -> f
	| ADD (e1, e2) -> galculator e1 +. galculator e2
 	| SUB (e1, e2) -> galculator e1 -. galculator e2
	| MUL (e1, e2) -> galculator e1 *. galculator e2
	| DIV (e1, e2) -> galculator e1 /. galculator e2
	| SIGMA (e1, e2, e3) ->  
		let e_1 = int_of_float (galculator e1) in
		let e_2 = int_of_float (galculator e2)
		in 
		if (e_1>e_2) then 0.
		else  ((galculator (SIGMA (INT e_1, INT (e_2 - 1), e3))) +.  subs (e3,float_of_int e_2))
	| INTEGRAL (e1, e2, e3) ->
		let e_1 = galculator e1 in
		let e_2 = galculator e2 in
		if(e_1>e_2) then (galculator (INTEGRAL(REAL e_2, REAL e_1, e3))) 
		else if(e_2-.e_1<0.1) then 0.
		else (galculator (INTEGRAL(REAL e_1, REAL (e_2-.0.1), e3)) +. 0.1*.subs (e3,e_2-.0.1)     )
and subs : exp * float -> float = fun (e,f) ->
match e with
| X -> f
| INT n-> float_of_int n
| REAL f_ -> f_
| ADD (e1, e2) -> subs (e1, f) +. subs (e2, f)
| SUB (e1, e2) -> subs (e1, f) -. subs (e2, f)
| MUL (e1, e2) -> subs (e1, f) *. subs (e2, f)
| DIV (e1, e2) -> subs (e1, f) /. subs (e2, f)
| SIGMA (e1, e2, e3) ->  galculator e
| INTEGRAL (e1, e2, e3) -> galculator e 


