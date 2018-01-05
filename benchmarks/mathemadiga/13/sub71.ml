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

let galculator exp = 	
	let rec galculator_inner exp env res = 
		match exp with	
		| X -> 
			(match env with
			| Some(x) -> x
			| None -> raise FreeVariable
			)
		| INT(i) -> float_of_int(i)
		| REAL(f) -> f
		| ADD(e1,e2) -> (galculator_inner e1 env res) +. (galculator_inner e2 env res)
		| SUB(e1,e2) -> (galculator_inner e1 env res) -. (galculator_inner e2 env res)
		| MUL(e1,e2) -> (galculator_inner e1 env res) *. (galculator_inner e2 env res)
		| DIV(e1,e2) -> (galculator_inner e1 env res) /. (galculator_inner e2 env res)
		| SIGMA(e1,e2,e3) -> 
			let s = galculator_inner e1 env 0.0 in
			let e = galculator_inner e2 env 0.0 in
			let int_s = int_of_float(s) in
			let int_e = int_of_float(e) in
			if int_s>int_e then res
			else (
				let r = (galculator_inner e3 (Some(s)) 0.0) in
				galculator_inner (SIGMA((INT (int_of_float(s)+1)),e2,e3)) env (res +. r)
			)
		| INTEGRAL(e1,e2,e3) ->
			let s = galculator_inner e1 env 0.0 in
			let e = galculator_inner e2 env 0.0 in
			let checker = abs_float(s-.e) in
			if checker<0.1 then res
			else if s>e
			then (
				let r = 0.1 *. (galculator_inner e3 (Some(s-.0.1)) 0.0) in
				galculator_inner (INTEGRAL((REAL (s-.0.1)),e2,e3)) env (res -. r)
			)
			else (
				let r = 0.1 *. (galculator_inner e3 (Some(s)) 0.0) in
				galculator_inner (INTEGRAL((REAL (s+.0.1)),e2,e3)) env (res +. r)
			)
	in

	galculator_inner exp None 0.0
