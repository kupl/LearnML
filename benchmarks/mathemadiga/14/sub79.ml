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

let rec inner_calculator (eq, x) =
	match eq with
	| X -> inner_calculator (x,x)	
	| INT n -> float_of_int n
	| REAL fl ->  fl
	| ADD (a,b) -> inner_calculator (a,x) +. inner_calculator (b,x)
	| SUB (a,b) -> inner_calculator (a,x) -. inner_calculator (b,x)
	| MUL (a,b) -> inner_calculator (a,x) *. inner_calculator (b,x)
	| DIV (a,b) -> inner_calculator (a,x) /. inner_calculator (b,x) 
	| SIGMA(a,b,eq) ->
		let new_a = inner_calculator (a,x) in
		let new_b = inner_calculator (b,x) in
		if(new_a <= new_b) then inner_calculator( ADD (eq , SIGMA(REAL( new_a +. 1.) , REAL new_b , eq) ) ,REAL new_a) else 0.
	| INTEGRAL(a,b,eq) ->
		let new_a = inner_calculator (a,x) in
		let new_b = inner_calculator (b,x) in
		if(new_b >= new_a +. 0.1) then inner_calculator (eq, REAL new_a)*. 0.1 +. inner_calculator(INTEGRAL(REAL (new_a+. 0.1) ,REAL new_b , eq)  ,REAL (new_a+. 0.1))
		else 0.

let rec galculator ex =
	match ex with
	| X -> raise (FreeVariable)
	| INT n -> float_of_int n
	| REAL fl ->  fl
	| ADD (a,b) -> galculator a +. galculator b
	| SUB (a,b) -> galculator a -. galculator b
	| MUL (a,b) -> galculator a *. galculator b
	| DIV (a,b) -> galculator a /. galculator b (* what do you do when b is zero? *)
	| SIGMA(a,b,eq) ->
		if(int_of_float (galculator a) <= int_of_float (galculator b)) then inner_calculator( ADD (eq , SIGMA( ADD ( a, INT 1) , b , eq) ) , a) else 0.
	| INTEGRAL(a,b,eq) ->
		let delta = (galculator b -. galculator a) in
		if(delta >= 0.1) then inner_calculator (eq,a)*. 0.1  +. inner_calculator(INTEGRAL( ADD ( a, REAL 0.1) , b , eq) ,  ADD(a, REAL 0.1)) 
		else if (delta <= -0.1) then -. (galculator (INTEGRAL(b,a,eq)))
		else 0.
