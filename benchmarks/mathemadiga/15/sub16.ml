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

let rec vcal e x =
	match e with
          X -> x
	| INT i -> float_of_int i
	| REAL r -> r
	| ADD (e1, e2) -> (vcal e1 x) +. (vcal e2 x)
	| SUB (e1, e2) -> (vcal e1 x) -. (vcal e2 x)
	| MUL (e1, e2) -> (vcal e1 x) *. (vcal e2 x)
	| DIV (e1, e2) -> (vcal e1 x) /. (vcal e2 x)
	| SIGMA (e1, e2, e3) ->
		let a = int_of_float (vcal e1 x) in
		let b = int_of_float (vcal e2 x) in
		if (a > b) then 0.0
		else if (a == b) then (vcal e3 (float_of_int b))
		else (vcal e3 (float_of_int a)) +. (vcal (SIGMA (INT (a+1), INT b, e3)) x)
	| INTEGRAL (e1, e2, e3) ->
		let a = vcal e1 x in
		let b = vcal e2 x in
		if (a > b) then -.(vcal(INTEGRAL(e2, e1, e3)) x)
		else if (a +. 0.1 > b) then 0.0
		else ((vcal e3 a) *. 0.1) +. (vcal (INTEGRAL (REAL (a+.0.1), REAL b, e3)) x)

let rec galculator e : float =
	match e with
	  X -> raise FreeVariable
	| INT i -> float_of_int i
	| REAL r -> r
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, e3) -> 
		let a = int_of_float (galculator e1) in
		let b = int_of_float (galculator e2) in
		if (a > b) then 0.0
		else if (a == b) then (vcal e3 (float_of_int b))
		else (vcal e3 (float_of_int a)) +. (galculator (SIGMA (INT (a+1), INT b, e3)))
	| INTEGRAL (e1, e2, e3) ->
		let a = galculator e1 in
		let b = galculator e2 in
		if (a > b) then -.(galculator(INTEGRAL(e2, e1, e3)))
		else if (a +. 0.1 > b) then 0.0
		else ((vcal e3 a) *. 0.1) +. (galculator (INTEGRAL (REAL (a+.0.1), REAL b, e3)))


let test1 = SIGMA( INT 1, INT 10, MUL(X,X) ) 
 let test2 = SIGMA( INT 1, INT 10, ADD( SIGMA (INT 1, X, X) , X) ) 
 let test3 = SIGMA( INT 1, INT 20, SIGMA( INT 1, MUL(X,X) , X ) ) 

 let _= print_string "test1 :" ; print_float (galculator test1); print_string "\n";; 
 let _= print_string "test2 :" ; print_float (galculator test2); print_string "\n";; 
 let _= print_string "test3 :" ; print_float (galculator test3); print_string "\n";; 
