(* KIHWAN KANG HW02-1 *)

(* PREDEFINED TYPES *)
type exp =
	X
	|INT of int
	|REAL of float
	|ADD of exp * exp
	|SUB of exp * exp
	|MUL of exp * exp
	|DIV of exp * exp
	|SIGMA of exp * exp * exp
	|INTEGRAL of exp * exp * exp
(* END OF PREDEFINED TYPES *)

exception FreeVariable

let rec galculator expr = 
	let rec solveX (exprX, const) = (
		match exprX with
		|X -> const
		|INT number -> float_of_int number
		|REAL number -> number
		|ADD (expr1, expr2) -> solveX (expr1, const) +. solveX (expr2, const)
		|SUB (expr1, expr2) -> solveX (expr1, const) -. solveX (expr2, const)
		|MUL (expr1, expr2) -> solveX (expr1, const) *. solveX (expr2, const)
		|DIV (expr1, expr2) -> 
			let expr2_solX = solveX (expr2, const) in
			if expr2_solX = 0.
				then raise Division_by_zero
				else solveX (expr1, const) /. expr2_solX
		|SIGMA (expri, exprn, exprXin) -> 
			galculator (SIGMA
				(
				(INT (int_of_float (solveX (expri, const)))),
				(INT (int_of_float (solveX (exprn, const)))),
				exprXin))
		|INTEGRAL (exprL, exprU, exprXin) -> 
			galculator (INTEGRAL 
				(
				(REAL (solveX (exprL, const))), 
				(REAL (solveX (exprU, const))), 
				exprXin))
	)
in
	match expr with
	|X -> raise FreeVariable
	|INT number -> float_of_int number
	|REAL number -> number
	|ADD (expr1, expr2) -> (galculator expr1) +. (galculator expr2)
	|SUB (expr1, expr2) -> (galculator expr1) -. (galculator expr2)
	|MUL (expr1, expr2) -> (galculator expr1) *. (galculator expr2)
	|DIV (expr1, expr2) -> 
		let expr2_galc = galculator expr2 in
		if expr2_galc = 0.
			then raise Division_by_zero
			else (galculator expr1) /. (galculator expr2)
	|SIGMA (expri, exprn, exprX) -> 
		let expri_galc = int_of_float (galculator expri) in
		let exprn_galc = int_of_float (galculator exprn) in
		if expri_galc > exprn_galc
			then 0.
			else (solveX (exprX, float_of_int expri_galc)) 
				+. (galculator (SIGMA ((INT (expri_galc + 1)), (INT exprn_galc), exprX)))
	|INTEGRAL (exprL, exprU, exprX) ->
		let exprL_galc = galculator exprL in
		let exprU_galc = galculator exprU in
		if exprL_galc > exprU_galc
			then galculator (INTEGRAL (REAL exprU_galc, REAL exprL_galc, MUL(INT (-1), exprX)))
		else if exprU_galc -. exprL_galc < 0.1
			then 0. 
			else (solveX (exprX, exprL_galc) *. 0.1) 
				+. (galculator (INTEGRAL ((REAL (exprL_galc +. 0.1)), (REAL exprU_galc), exprX)))
	
