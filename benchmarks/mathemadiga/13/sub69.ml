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

let rec galculator expin =
	let rec eval(env,e) =
		match e with
		| X -> if env=[] then raise FreeVariable
				else (List.hd env)
		| INT n -> float n
		| REAL r -> r
		| ADD(e1,e2) -> eval(env,e1) +. eval(env,e2)
		| SUB(e1,e2) -> eval(env,e1) -. eval(env,e2)
		| MUL(e1,e2) -> eval(env,e1) *. eval(env,e2)
		| DIV(e1,e2) -> eval(env,e1) /. eval(env,e2)
		| SIGMA(e1,e2,e3) -> if (int_of_float (eval(env,e1))) > (int_of_float (eval(env,e2))) then 0.
							else eval((eval(env,INT (int_of_float (eval(env,e1)))))::env,e3) +. eval(env,SIGMA(INT ((int_of_float (eval(env,e1))) + 1),INT (int_of_float (eval(env,e2))),e3))
		| INTEGRAL(e1,e2,e3) -> if (eval(env,e1)-.eval(env,e2) > -0.1)&&(eval(env,e1)-.eval(env,e2) < 0.1) then 0.
								else if eval(env,e1)>eval(env,e2) then ((-0.1) *. eval((eval(env,e1))::env,e3)) +. eval(env,INTEGRAL(REAL (eval(env,e1) -. 0.1),REAL (eval(env,e2)),e3))
								else (0.1 *. eval((eval(env,e1))::env,e3)) +. eval(env,INTEGRAL(REAL (eval(env,e1) +. 0.1),REAL (eval(env,e2)),e3))
		in							
	eval([],expin)
	