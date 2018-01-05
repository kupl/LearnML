exception FreeVariable
type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp
let rec galculator : exp -> float = 
	let rec f : float * exp * bool -> float = 
		fun(x, expr, en) -> match expr with
		| X ->
			if en == true then x
			else raise FreeVariable
		| INT arg -> float_of_int arg
		| REAL arg -> arg
		| ADD(arg1, arg2) -> f(x, arg1, en) +. f(x, arg2, en)
		| SUB(arg1, arg2) -> f(x, arg1, en) -. f(x, arg2, en)
		| MUL(arg1, arg2) -> f(x, arg1, en) *. f(x, arg2, en)
		| DIV(arg1, arg2) -> f(x, arg1, en) /. f(x, arg2, en)
		| SIGMA(a, b, expr) ->
			let a_float : float = f(x, a, en) in
			let b_float : float = f(x, b, en) in
			if a_float > b_float then 0.0
			else 
				f(floor a_float, expr, true) +. f(x,(SIGMA(ADD(a,INT 1),b,expr)), en)
		| INTEGRAL(a, b, expr) ->
			let a_float : float = f(x, a, en) in
			let b_float : float = f(x, b, en) in
			if b_float < a_float then -. (f(x, INTEGRAL(b, a, expr), en))
			else if b_float < (a_float +. 0.1) then 0.0
			else 
				(f(a_float, expr, true) /. 10.0) +. (f(x, INTEGRAL(REAL (a_float +. 0.1), b, expr), en))
	in
	fun expr -> f(0.0, expr, false)
