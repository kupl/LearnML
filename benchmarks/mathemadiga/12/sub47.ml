type exp = X
		   | INT of int
		   | REAL of float
		   | ADD of exp * exp
		   | SUB of exp * exp
		   | MUL of exp * exp
		   | DIV of exp * exp
		   | SIGMA of exp * exp * exp
		   | INTEGRAL of exp * exp * exp
		   
exception NO_ENV of string

let mathemadiga e =
	let rec cal e env = 
		match (e, env) with
		| (X, x::[]) -> x
		| (X, _) -> raise (NO_ENV "no environment of x")
		| (INT(x), _) -> float_of_int x
		| (REAL(x), _) -> x
		| (ADD(e1, e2), _) -> cal e1 env +. cal e2 env
		| (SUB(e1, e2), _) -> cal e1 env -. cal e2 env
		| (MUL(e1, e2), _) -> cal e1 env *. cal e2 env
		| (DIV(e1, e2), _) -> cal e1 env /. cal e2 env
		| (SIGMA(low, high, e), _) ->
			let low_val = int_of_float(cal low env)
			and high_val = int_of_float(cal high env)
			in
			if low_val <= high_val then
				(cal e ((float_of_int low_val)::[])) +. (cal (SIGMA(ADD(low, REAL 1.0), high, e)) env)
			else 0.0
		| (INTEGRAL(low, high, e), _) ->
			let low_val = cal low env
			and high_val = cal high env
			in
			if low_val > high_val then
				~-. (cal (INTEGRAL(high, low, e)) env)
			else if low_val +. 0.1 <= high_val then
				(cal e ((low_val)::[])) *. 0.1 +. (cal (INTEGRAL(ADD(low, REAL 0.1), high, e)) env)
			else
				(cal e ((low_val)::[])) *. (high_val -. low_val)
	in
	cal e []