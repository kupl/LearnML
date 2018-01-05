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

let rec galculator ex = 
	match ex with
	| X -> raise FreeVariable
	| INT(i) -> float_of_int(i)
	| REAL(r) -> r
	| ADD(x,y) -> galculator(x) +. galculator(y)
	| SUB(x,y) -> galculator(x) -. galculator(y)
	| MUL(x,y) -> galculator(x) *. galculator(y)
	| DIV(x,y) -> galculator(x) /. galculator(y)
	| SIGMA(x,y,f) -> 
		let a = int_of_float(galculator(x)) and
		b = int_of_float(galculator(y)) in
		if a > b then 0.0
		else eval(f,x) +. galculator(SIGMA(ADD(x,INT(1)),y,f))
	| INTEGRAL(x,y,f) ->
		let a = galculator(x) and
		b = galculator(y) in
		if abs_float(a-.b) < 0.1 then 0.0
		else if a -. b > 0.0 then -.galculator(INTEGRAL(y,x,f))
		else 0.1 *. eval(f,x) +. galculator(INTEGRAL(ADD(REAL(0.1),x),y,f))
and eval (f,x) = 
	match f with
	| X -> galculator(x)
	| INT(i) -> float_of_int(i)
	| REAL(r) -> r
	| ADD(g,h) -> eval(g,x) +. eval(h,x)
	| SUB(g,h) -> eval(g,x) -. eval(h,x)
	| MUL(g,h) -> eval(g,x) *. eval(h,x)
	| DIV(g,h) -> eval(g,x) /. eval(h,x)
	| SIGMA(a,b,g) -> galculator(SIGMA(REAL(eval(a,x)),REAL(eval(b,x)),g))
	| INTEGRAL(a,b,g) -> galculator(INTEGRAL(REAL(eval(a,x)),REAL(eval(b,x)),g))